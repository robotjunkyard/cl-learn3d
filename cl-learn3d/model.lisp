(in-package :cl-learn3d)

;; this will later include other info like color
(defstruct meshface
  ;;; multiple-of-3-length array of integers referring to the INDEX of a vertex from host mesh's vertexdata
  (vertices (make-array 0 :element-type 'uint32 :initial-element 0 :adjustable t :fill-pointer 0) :type (vector uint32 *)))

(defstruct material
  (name (error "need a name for material") :type string :read-only t)
  (color (make-colori) :type colori))

(defparameter *default-triangle-material*
  (make-material :name "Default"
		 :color (make-colori :r 128 :g 160 :b 160)))

(defstruct mesh
  ;;; a multiple-of-3-length array containing x1,y1,z1,x2,y2,z2,...xn,yn,zn where n = # vertices
  (vertexdata (make-array 0 :element-type 'single-float :adjustable t :fill-pointer 0)
	      :type (vector single-float))
  (faces (make-array 0 :element-type 'meshface :adjustable t :fill-pointer 0)
	 :type (VECTOR T *))
  (face-materials (make-array 0 :element-type 'meshface :adjustable t :fill-pointer 0)
			 :type (VECTOR T *))
  (mtllib #() :type (SIMPLE-VECTOR *))  ;; created from result of LOAD-MTLLIB function
  (face-material-indices (make-array 0 :element-type 'int8) :type (SIMPLE-ARRAY INT8 (*)))
  
  ;;;; Dynamically-altered data below
  ;; vertexdata-* is the "output" of transformation operations done from vertexdata
  (vertexdata-* (make-array 0 :element-type 'single-float :adjustable t :fill-pointer 0)
	       :type (vector single-float))
  ;; used, and continuously clobbered per call, by sorted model draw function
  (face-draw-order (make-array 0 :element-type 'uint32 :adjustable t :fill-pointer 0)
		   :type (vector uint32))
  ;;(face-visibility-bits (make-array 0 :element-type 'bit :adjustable t :fill-pointer 0)
  ;;:type (vector bit)
			)
(defun find-material-index (mesh material-name)
  (declare (type mesh mesh)
	   (type string material-name))
  (loop for m across (mesh-mtllib mesh)
     for i of-type int16 from 0
     do (when (string= (material-name m) material-name)
	  (return-from find-material-index i)))
  -1)

(defun load-model (name)
  (declare (type string name))
  (format t "Loading model '~a'~%" name)
  (let* ((filename (pathname (concatenate 'string 
					 "./models/"
					 name
					 ".obj")))
	 (lines (tokenize-text-file-into-lines filename))
	 (vertexdata (make-array 0 :element-type 'single-float
				 :adjustable t
				 :fill-pointer 0))
	 (vertexcount 0)
	 (facecount   0)
	 (mtllib-filename nil)
	 (usemtl-table ())
	 (faces (make-array 0 :element-type 'meshface :adjustable t :fill-pointer 0)))
    (declare (type uint32 vertexcount facecount))
    (loop for line in lines do
	 (let ((tokens (split-sequence:split-sequence #\Space line)))
	   (case-using
	       #'equal (first tokens)
	     ("mtllib"  (format t "MtlLib          :    ~a~%" (setf mtllib-filename (car (cdr tokens)))))
	     ("usemtl"  (let ((mtlname (car (cdr tokens))))
			  (format t "UseMtl          :    ~a (@~d)~%"  mtlname facecount)
			  (setf usemtl-table (append usemtl-table (list facecount mtlname)))))
	     ("o"       (format t "Object (ignored):    ~a~%" (cdr tokens)))  ;; is 'o' directive even useful??
	     ("v" (let ((x (read-from-string (first (cdr tokens))))
			(y (read-from-string (second (cdr tokens))))
			(z (read-from-string (third (cdr tokens)))))
		    (declare (type single-float x y z))
		    (vector-push-extend x vertexdata)
		    (vector-push-extend y vertexdata)
		    (vector-push-extend z vertexdata))
		  (incf (the uint32 vertexcount)))
	     ("vn" nil)   ;; vnormals are not used in this demo
	     ("vt" nil)   ;; uv coords are not used in this demo (not until texturing implemented, at least)
	     ("f" (let ((face (make-meshface)))
		    (loop for v of-type string in (cdr tokens) do
			 (let ((s (subst NIL "" (split-sequence:split-sequence #\/ v)
					 :test #'equal)))
			   (vector-push-extend
			    (1- (read-from-string (first s)))
			    (meshface-vertices face))))
		    (incf (the uint32 facecount))
		    (vector-push-extend face faces)))
	     ("s" nil)   ;; not 100% sure what this is.  ignoring it never caused problems
	     )))
    (if (> (the uint32 (length vertexdata)) 0)
	(let ((newmodel
	       (make-mesh :vertexdata vertexdata
			  :vertexdata-* (copy-seq vertexdata)
			  :faces faces
			  :face-draw-order (make-array facecount :element-type 'uint32
						       :initial-contents (loop for i below facecount collect i)))))
	  (format t "Mesh loaded with ~d vertices and ~d faces.~%" vertexcount facecount)
	  (when mtllib-filename
	    (setf (mesh-mtllib newmodel) (load-mtllib mtllib-filename))
	    (setf (mesh-face-material-indices newmodel)
		  (make-array facecount :element-type 'int8 :initial-element 0))
	    (let ((mtl-i -1))
	      (loop for i from 0 below (length (mesh-faces newmodel)) do
		   (let ((new-mtl-name (getf usemtl-table i)))  ;; first check if we're supposed to assign diff mtl
		     (when new-mtl-name  ;; string or nil
		       (let ((new-mtl-index (find-material-index newmodel new-mtl-name)))
			 (when (<= 0 new-mtl-index)
			   (setf mtl-i new-mtl-index)))))
		   (setf (aref (mesh-face-material-indices newmodel) i) mtl-i))))
	    
	  ;;;;(loop repeat facecount do
	  ;;;;     (vector-push-extend 1 (mesh-face-visibility-bits newmodel)))
	    newmodel)
	(progn (format t "Warning: 0 vertices loaded in load-model for file '~a', returning NIL!!"
		       filename)
	       NIL))))

  (defun mesh-vertex-coords (mesh vertex#)
    "Returns an X,Y,Z triple-value of the coordinates of a requested vertex of a mesh."
  (declare (type mesh mesh)
	   (type uint16 vertex#))
  (let ((array-idx (* vertex# 3)))
    (declare (type uint32 array-idx))
    (values (aref (mesh-vertexdata mesh) array-idx)
	    (aref (mesh-vertexdata mesh) (+ 1 array-idx))
	    (aref (mesh-vertexdata mesh) (+ 2 array-idx)))))

(defun mesh-face-coords (mesh face#)
  "Returns a 9-value set of vertex coordinates x1,y1,z1,x2,y2,z2,x3,y3,z3 of a requested face of a mesh."
  (declare (type mesh mesh)
	   (type uint32 face#))
  (let ((meshface (aref (mesh-faces mesh) face#)))
    (declare (type meshface meshface))
    (let ((v0idx (aref (meshface-vertices meshface) 0))
	  (v1idx (aref (meshface-vertices meshface) 1))
	  (v2idx (aref (meshface-vertices meshface) 2)))
      (declare (type uint32 v0idx v1idx v2idx))
      (let ((ar0i (* v0idx 3))
	    (ar1i (* v1idx 3))
	    (ar2i (* v2idx 3)))
	(declare (type uint32 ar0i ar1i ar2i))
	(values (aref (mesh-vertexdata mesh) ar0i)
		(aref (mesh-vertexdata mesh) (+ 1 ar0i))
		(aref (mesh-vertexdata mesh) (+ 2 ar0i))
		(aref (mesh-vertexdata mesh) ar1i)
		(aref (mesh-vertexdata mesh) (+ 1 ar1i))
		(aref (mesh-vertexdata mesh) (+ 2 ar1i))
		(aref (mesh-vertexdata mesh) ar2i)
		(aref (mesh-vertexdata mesh) (+ 1 ar2i))
		(aref (mesh-vertexdata mesh) (+ 2 ar2i)))))))

(defun mesh-face-coords-* (mesh face#)
  "Returns a 9-value set of TRANSFORMED vertex coordinates (ergo: from vertexdata-*, not vertexdata) x1,y1,z1,x2,y2,z2,x3,y3,z3 of a requested face of a mesh."
  (declare (type mesh mesh)
	   (type uint32 face#))
  (let ((meshface (aref (mesh-faces mesh) face#)))
    (declare (type meshface meshface))
    (let ((v0idx (aref (meshface-vertices meshface) 0))
	  (v1idx (aref (meshface-vertices meshface) 1))
	  (v2idx (aref (meshface-vertices meshface) 2)))
      (declare (type uint32 v0idx v1idx v2idx))
      (let ((ar0i (* v0idx 3))
	    (ar1i (* v1idx 3))
	    (ar2i (* v2idx 3)))
	(declare (type uint32 ar0i ar1i ar2i))
	(values (aref (mesh-vertexdata-* mesh) ar0i)
		(aref (mesh-vertexdata-* mesh) (+ 1 ar0i))
		(aref (mesh-vertexdata-* mesh) (+ 2 ar0i))
		(aref (mesh-vertexdata-* mesh) ar1i)
		(aref (mesh-vertexdata-* mesh) (+ 1 ar1i))
		(aref (mesh-vertexdata-* mesh) (+ 2 ar1i))
		(aref (mesh-vertexdata-* mesh) ar2i)
		(aref (mesh-vertexdata-* mesh) (+ 1 ar2i))
		(aref (mesh-vertexdata-* mesh) (+ 2 ar2i)))))))

(defun draw-mesh (mesh renderer)
  (declare (type mesh mesh))
  (sort-mesh-face-draw-order mesh *world-matrix*)
  (loop
     for face# across (mesh-face-draw-order mesh)
     for material = (determine-material-for-face mesh face#)
     ;; for fc of-type uint32 from 0 do
     do
       ;;; (format t "~d, " face#)
       (multiple-value-bind (x1 y1 z1 x2 y2 z2 x3 y3 z3)
	   (mesh-face-coords-* mesh face#)
	 (draw-3d-triangle-* x1 y1 z1 x2 y2 z2 x3 y3 z3 *world-matrix* renderer
			     :color (material-color material)
			     ))))

(defun dist-points-nosqrt (x1 y1 z1 x2 y2 z2)
  (declare (type single-float x1 y1 z1 x2 y2 z2))
  "Used strictly for comparisons; so square-root is not important."
  (let ((result (+ (expt (- x1 x2) 2)
		   (expt (- y1 y2) 2)
		   (expt (- z1 z2) 2))))
    (declare (type single-float result))
    result))

(defun tri-sort-valuator (mesh face# tmat)
  "Returns distance between model (after local&world transforms) and the camera's origin."
  (declare (type mesh mesh)
	   (type uint32 face#)
	   (type mat4x4 tmat)
	   (optimize (speed 3) (safety 1)))
  (let ((cx (aref *camera-eye* 0))
	(cy (aref *camera-eye* 1))
	(cz (aref *camera-eye* 2)))
    (declare (type single-float cx cy cz))
    (multiple-value-bind (fx1 fy1 fz1 fx2 fy2 fz2 fx3 fy3 fz3)
	(mesh-face-coords-* mesh face#)
      (declare (type single-float fx1 fy1 fz1 fx2 fy2 fz2 fx3 fy3 fz3))
      (let* (;; yeah I guess the camera eye is supposed to be transformed too...
	     ;; since results are visually less "mistakey" this way than with just
	     ;; the unaltered coordinates.  gonna have to hit the books to be certain...
	     (tcp  (sb-cga:transform-point (sb-cga:vec (- cx) (- cy) (- cz)) tmat))    
	     (tfp1 (sb-cga:transform-point (sb-cga:vec fx1 fy1 fz1) tmat))
	     (tfp2 (sb-cga:transform-point (sb-cga:vec fx2 fy2 fz2) tmat))
	     (tfp3 (sb-cga:transform-point (sb-cga:vec fx3 fy3 fz3) tmat))
	     (tfx1 (aref tfp1 0))
	     (tfy1 (aref tfp1 1))
	     (tfz1 (aref tfp1 2))
	     (tfx2 (aref tfp2 0))
	     (tfy2 (aref tfp2 1))
	     (tfz2 (aref tfp2 2))
	     (tfx3 (aref tfp3 0))
	     (tfy3 (aref tfp3 1))
	     (tfz3 (aref tfp3 2)))
	(declare (type mat4x4 tmat)
		 (type vec3 tcp tfp1 tfp2 tfp3)
		 (type single-float tfx1 tfy1 tfz1 tfx2 tfy2 tfz2 tfx3 tfy3 tfz3)
		 (dynamic-extent tcp tfp1 tfp2 tfp3))
	(dist-points-nosqrt (aref tcp 0)
			    (aref tcp 1)
			    (aref tcp 2)
			    (min tfx1 tfx2 tfx3)
			    (min tfy1 tfy2 tfy3)
			    (min tfz1 tfz2 tfz3))))))

(defun sort-mesh-face-draw-order (mesh tmat)
  "In-place sorts the FACE-DRAW-ORDER array of a given mesh object given a transformation matrix 'TMATRIX' and vector 'CAMERA-ORIGIN'"
  (declare (type mesh mesh))
  (quicksort-fv (mesh-face-draw-order mesh)
		0 (1- (length (mesh-face-draw-order mesh)))
		:valuator #'(lambda (face#)
			      (declare (type uint32 face#))
			      (tri-sort-valuator mesh face# tmat))))

(defun transform-model (mesh mat)
  "Update a model's vertexdata-* field to be the transformed version of its static vertex data, given the passed matrix."
  (declare (type mesh mesh)
	   (type mat4x4 mat))
  (loop
     for vi of-type uint32 below (length (mesh-vertexdata mesh)) by 3
     ;; this loop probably conses like a donkey... TODO: optimize this later, AFTER it works
     for vert = (sb-cga:vec (aref (mesh-vertexdata mesh) vi)
			    (aref (mesh-vertexdata mesh) (+ 1 vi))
			    (aref (mesh-vertexdata mesh) (+ 2 vi)))
     for tv   = (sb-cga:transform-point vert mat)
     do
       (setf (aref (mesh-vertexdata-* mesh) vi)       (aref tv 0)
	     (aref (mesh-vertexdata-* mesh) (+ 1 vi)) (aref tv 1)
	     (aref (mesh-vertexdata-* mesh) (+ 2 vi)) (aref tv 2)))
  nil)

(defun load-mtllib (filename)
  (let* ((pathstring (pathname (concatenate 'string 
					    "./models/"
					    filename)))
	 (lines (tokenize-text-file-into-lines pathstring))
	 (materials ())
	 (current-material nil))
    (loop for line in lines do
	 (let ((tokens (split-sequence:split-sequence #\Space line)))
	   (case-using
	       #'equal (first tokens)
	     ("newmtl" (setf current-material
			     (make-material :name (car (cdr tokens)))
			     materials
			     (append materials (list current-material))))
	     ("Kd"     (let ((params (cdr tokens)))
			 (let* ((r (read-from-string (first params)))
				(g (read-from-string (second params)))
				(b (read-from-string (third params))))
			     (setf (material-color current-material)
				   (convert-colorf-to-colori (make-colorf :r r :g g :b b :a 1.0)))))))))
    (make-array (length materials)
		:element-type 'material
		:initial-contents materials)))
    
(defun determine-material-for-face (mesh face#)
  (declare (type mesh mesh)
	   (type uint32 face#))
  (let ((mi (aref (mesh-face-material-indices mesh) face#)))
    (declare (type int8 mi))
    (cond ((= -1 mi)
	   *default-triangle-material*)
	  (T (aref (mesh-mtllib mesh) mi)))))
