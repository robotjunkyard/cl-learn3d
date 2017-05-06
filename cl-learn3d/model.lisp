(in-package :cl-learn3d)

;; this will later include other info like color
(defstruct meshface
  ;;; multiple-of-3-length array of integers referring to the INDEX of a vertex from host mesh's vertexdata
  (vertices (make-array 0 :element-type 'uint32 :initial-element 0 :adjustable t :fill-pointer 0) :type (vector uint32 *)))

(defstruct mesh
  ;;; a multiple-of-3-length array containing x1,y1,z1,x2,y2,z2,...xn,yn,zn where n = # vertices
  (vertexdata (make-array 0 :element-type 'single-float :adjustable t :fill-pointer 0)
	      :type (vector single-float))
  (faces (make-array 0 :element-type 'meshface :adjustable t :fill-pointer 0)
	 :type (VECTOR T *)))

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
	 (faces (make-array 0 :element-type 'meshface :adjustable t :fill-pointer 0)))
    (declare (type uint32 vertexcount facecount))
    (loop for line in lines do
	 (let ((tokens (split-sequence:split-sequence #\Space line)))
	   (case-using
	    #'equal (first tokens)
	    ("mtllib"  (format t "MtlLib (ignored):    ~a~%" (cdr tokens)))
	    ("o"       (format t "Object (ignored):    ~a~%" (cdr tokens)))
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
			   (meshface-vertices face))
			  (incf (the uint32 facecount))
			  
			  ;; (when (second s)
			  ;;  (vector-push-extend
			  ;;   (1- (read-from-string (second s)))
			  ;;   (meshface-uv face)))
			  ;; (when (third s)
			  ;;  (vector-push-extend
			  ;;   (1- (read-from-string (third s)))
			  ;;   (meshface-normals face)))
			  ))
		   (vector-push-extend face faces)))
	    ("s" nil)   ;; not 100% sure what this is.  ignoring it never caused problems
	    )))
    (if (> (the uint32 (length vertexdata)) 0)
	(let ((newmodel
	       (make-mesh :vertexdata vertexdata
			  :faces faces)))
	  (format t "Mesh loaded with ~d vertices and ~d faces.~%" vertexcount facecount)
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

;; closure with a buffered triangle to plop the results for each triangle, to reduce consing
(let ((%tri (make-array 3 :initial-contents
	      (list
	       (sb-cga:vec -1.0  -1.0  0.0)
	       (sb-cga:vec  1.0  -1.0  0.0)
	       (sb-cga:vec  0.0   1.0  0.0)))))
  (defun draw-mesh (mesh renderer)
    (declare (type mesh mesh))
    (loop
       for face# from 0 below (length (mesh-faces mesh))
       for fc of-type uint32 from 0 do
	 (multiple-value-bind (x1 y1 z1 x2 y2 z2 x3 y3 z3)
	     (mesh-face-coords mesh face#)
	   (setf (aref (aref %tri 0) 0) x1
		 (aref (aref %tri 0) 1) y1
		 (aref (aref %tri 0) 2) z1
		 (aref (aref %tri 1) 0) x2
		 (aref (aref %tri 1) 1) y2
		 (aref (aref %tri 1) 2) z2
		 (aref (aref %tri 2) 0) x3
		 (aref (aref %tri 2) 1) y3
		 (aref (aref %tri 2) 2) z3)
	   (draw-3d-triangle %tri renderer)))))
