(in-package :cl-learn3d)

(defparameter *default-triangle-color*
  (make-colori :r 128 :g 160 :b 160))

(defun draw-3d-line (x1 y1 z1 x2 y2 z2 tmat renderer)
  (declare (type single-float x1 y1 z1 x2 y2 z2)
	   (type (simple-array single-float (16)) tmat))
  (let* ((p1 (sb-cga:vec x1 y1 z1))
	 (p2 (sb-cga:vec x2 y2 z2))
	 (tp1 (sb-cga:transform-point p1 tmat))
	 (tp2 (sb-cga:transform-point p2 tmat))
	 (sx1 (round (* (/ (1+ (aref tp1 0)) 2.0) *x-res*)))
	 (sy1 (round (* (/ (- 1 (aref tp1 1)) 2.0) *y-res*)))
	 (sx2 (round (* (/ (1+ (aref tp2 0)) 2.0) *x-res*)))
	 (sy2 (round (* (/ (- 1 (aref tp2 1)) 2.0) *y-res*))))
    (declare (type (simple-array single-float (3)) p1 p2 tp1 tp2)
	     (type int32 sx1 sy1 sx2 sy2))
    (sdl2:render-draw-line 
     renderer
     sx1 sy1 sx2 sy2)))

(defun draw-axes (renderer)
  (sdl2:set-render-draw-color renderer 255 0 0 255)
  (draw-3d-line 0.0 0.0 0.0 *axis-size* 0.0 0.0 *world-matrix* renderer)
  (sdl2:set-render-draw-color renderer 0 255 0 255)
  (draw-3d-line 0.0 0.0 0.0 0.0 *axis-size* 0.0 *world-matrix* renderer)
  (sdl2:set-render-draw-color renderer 0 0 255 255)
  (draw-3d-line 0.0 0.0 0.0 0.0 0.0 *axis-size* *world-matrix* renderer))

(defun draw-2d-filled-triangle (x1 y1 x2 y2 x3 y3 renderer
				&key (print-stats nil) (draw-debug nil)
				  (fill t) (wireframe nil)
				  (color *default-triangle-color*))
  (declare (type fixnum x1 y1 x2 y2 x3 y3)
	   (type boolean print-stats draw-debug fill wireframe)
	   (type colori color))
  (when draw-debug
    ;; draws Red, Green, Blue quads on vertices to visually indicate pre-sorted order
    (let ((rect (sdl2:make-rect (- x1 3) (- y1 3) 6 6)))
      (sdl2:set-render-draw-color renderer 255 0 0 255)
      (sdl2:render-fill-rect renderer rect)
      (setf (sdl2:rect-x rect) (- x2 3)
	    (sdl2:rect-y rect) (- y2 3))
      (sdl2:set-render-draw-color renderer 0 255 0 255)
      (sdl2:render-fill-rect renderer rect)
      (setf (sdl2:rect-x rect) (- x3 3)
	    (sdl2:rect-y rect) (- y3 3))
      (sdl2:set-render-draw-color renderer 0 128 255 255)
      (sdl2:render-fill-rect renderer rect)))
  (let ((topx x1) (topy y1)
	(midx x2) (midy y2)
	(btmx x3) (btmy y3))
    (declare (type fixnum topx topy midx midy btmx btmy))
    ;; first sort the vertices
    (when (> topy midy)
      (rotatef topx midx)
      (rotatef topy midy))
    (when (> topy btmy)
      (rotatef topx btmx)
      (rotatef topy btmy))
    (when (> midy btmy)
      (rotatef midx btmx)
      (rotatef midy btmy))  
    (when print-stats
      (format t "In: [~3d,~3d]        Sorted: [~3d,~3d]~%" x1 y1   topx topy)
      (format t "    [~3d,~3d]                [~3d,~3d]~%" x2 y2   midx midy)
      (format t "    [~3d,~3d]                [~3d,~3d]~%" x3 y3   btmx btmy))
    (sdl2:set-render-draw-color renderer (colori-r color) (colori-g color) (colori-b color) (colori-a color))
    (let ((midx-topx (- midx topx))
	  (btmx-topx (- btmx topx))
	  (btmx-midx (- btmx midx))
	  (midy-topy (- midy topy))
	  (btmy-topy (- btmy topy))
	  (btmy-midy (- btmy midy)))
      (declare (type fixnum midx-topx btmx-topx btmx-midx midy-topy btmy-topy btmy-midy))
      (when (zerop btmy-topy)
	(when print-stats
	  (format t " Fail A~%"))
	(return-from draw-2d-filled-triangle nil))
      ;; vertices now sorted
      ;; now check error conditions to prevent / by 0
      ;; now fill top sub-triangle
      (let ((dlong (/ (float btmx-topx) btmy-topy))
	    (dupper (/ (float midx-topx) midy-topy))
	    (dlower (/ (float btmx-midx) btmy-midy))
	    (sx0 (float topx))
	    (sx1 (float topx)))
	(when print-stats
	  (format t "  D[~6a, ~6a, ~6a]~%" dlong dupper dlower))
	(tagbody
	   (unless fill
	     (go DRAW-WIREFRAME))
	   (when (zerop midy-topy)
	     (setq sx0 midx)
	     (when print-stats
	       (format t " Skip Upper~%"))
	     (go DRAW-LOWER-TRIANGLE))
	   DRAW-UPPER-TRIANGLE
	   (loop
	      for y from topy below midy
	      do
		(sdl2:render-draw-line renderer (truncate sx0) y (truncate sx1) y)
		(incf sx0 dupper)
		(incf sx1 dlong))
	   (when (zerop btmy-midy)
	     (when print-stats
	       (format t " Skip Lower~%"))
	     (go DRAW-WIREFRAME))
	   DRAW-LOWER-TRIANGLE
	   (loop
	      for y from midy below btmy
	      do
		(sdl2:render-draw-line renderer (truncate sx0) y (truncate sx1) y)
		(incf sx0 dlower)
		(incf sx1 dlong))
	 DRAW-WIREFRAME
	   (when wireframe
	     (sdl2:set-render-draw-color renderer 255 255 255 255)
	     (sdl2:render-draw-line renderer x1 y1 x2 y2)
	     (sdl2:render-draw-line renderer x2 y2 x3 y3)
	     (sdl2:render-draw-line renderer x3 y3 x1 y1))
	   ))))
  nil)

#|(let ((coords (make-array 6 :element-type
			  '(signed-byte 32))))
  (defun draw-3d-triangle (tri renderer &key (fill t) (wireframe nil)
					    (color *default-triangle-color*))
    (declare (type boolean fill wireframe)
	     ;;;; (type (single-array single-float (16)) tmat)
	     (type colori color))
    (loop for i below 3 do
	 (let* ((tmat (sb-cga:matrix* *pmat* *vmat* *rotmat*))
		(tv (sb-cga:transform-point 
		     (aref tri i)
		     tmat))
		;;(tv (aref tri i))
		;; translate to screen coordinates
		(x (* (/ (1+ (aref tv 0)) 2.0) *x-res*))
		(y (* (/ (- 1 (aref tv 1)) 2.0) *y-res*)))
	   (setf (aref coords (* 2 i))       (round x)
		 (aref coords (1+ (* 2 i)))  (round y))))
      (sdl2:set-render-draw-color renderer 207 205 155 255)
      (draw-2d-filled-triangle (aref coords 0) (aref coords 1)
			       (aref coords 2) (aref coords 3)
			       (aref coords 4) (aref coords 5)
			       renderer
			       :wireframe t :fill t)))|#

(defun draw-3d-triangle-* (x1 y1 z1 x2 y2 z2 x3 y3 z3 tmat
			   renderer &key (fill t) (wireframe t)
				      (color *default-triangle-color*))
  (declare (type single-float x1 y1 x2 y2 x3 y3)
	   (type (simple-array single-float (16)) tmat)
	   (type boolean fill wireframe)
	   (type colori color))
  (let* ((tv1 (sb-cga:transform-point (sb-cga:vec x1 y1 z1) tmat))
	 (tv2 (sb-cga:transform-point (sb-cga:vec x2 y2 z2) tmat))
	 (tv3 (sb-cga:transform-point (sb-cga:vec x3 y3 z3) tmat))
	 
	 ;; project 'em to 2D screen coordinates
	 (tx1 (* (/ (1+ (aref tv1 0)) 2.0) *x-res*))
	 (ty1 (* (/ (- 1 (aref tv1 1)) 2.0) *y-res*))
	 (tx2 (* (/ (1+ (aref tv2 0)) 2.0) *x-res*))
	 (ty2 (* (/ (- 1 (aref tv2 1)) 2.0) *y-res*))
	 (tx3 (* (/ (1+ (aref tv3 0)) 2.0) *x-res*))
	 (ty3 (* (/ (- 1 (aref tv3 1)) 2.0) *y-res*)))
    (declare (type (simple-array single-float (3)) tv1 tv2 tv3)
	     (type single-float tx1 ty1 tx2 ty2 tx3 ty3))

    (sdl2:set-render-draw-color renderer (colori-r color)
				(colori-g color) (colori-b color)
				(colori-a color))
    (draw-2d-filled-triangle (truncate tx1) (truncate ty1)
			     (truncate tx2) (truncate ty2)
			     (truncate tx3) (truncate ty3)
			     renderer
			     :wireframe wireframe :fill fill)))

