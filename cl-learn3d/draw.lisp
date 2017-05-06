(in-package :cl-learn3d)

(defun draw-line (x1 y1 z1 x2 y2 z2 renderer)
  (let* ((p1 (sb-cga:vec x1 y1 z1))
	 (p2 (sb-cga:vec x2 y2 z2))
	 (tmat (sb-cga:matrix* *pmat* *vmat* *rotmat*))
	 (tp1 (sb-cga:transform-point p1 tmat))
	 (tp2 (sb-cga:transform-point p2 tmat))
	 (sx1 (round (* (/ (1+ (aref tp1 0)) 2.0) *x-res*)))
	 (sy1 (round (* (/ (- 1 (aref tp1 1)) 2.0) *y-res*)))
	 (sx2 (round (* (/ (1+ (aref tp2 0)) 2.0) *x-res*)))
	 (sy2 (round (* (/ (- 1 (aref tp2 1)) 2.0) *y-res*))))
    (sdl2:render-draw-line 
     renderer
     sx1 sy1 sx2 sy2)))

(defun draw-axes (renderer)
  (sdl2:set-render-draw-color renderer 255 0 0 255)
  (draw-line 0.0 0.0 0.0 *axis-size* 0.0 0.0 renderer)
  (sdl2:set-render-draw-color renderer 0 255 0 255)
  (draw-line 0.0 0.0 0.0 0.0 *axis-size* 0.0 renderer)
  (sdl2:set-render-draw-color renderer 0 0 255 255)
  (draw-line 0.0 0.0 0.0 0.0 0.0 *axis-size* renderer))

(defun draw-2d-filled-triangle (x1 y1 x2 y2 x3 y3 renderer &key (print-stats nil))
  (declare (type fixnum x1 y1 x2 y2 x3 y3)
	   (type boolean print-stats))
  ;; this will be drawing li
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
    (let ((midx-topx (- midx topx))
	  (btmx-topx (- btmx topx))
	  (btmx-midx (- btmx midx))
	  (midy-topy (- midy topy))
	  (btmy-topy (- btmy topy))
	  (btmy-midy (- btmy midy)))
      (declare (type fixnum midx-topx btmx-topx btmx-midx midy-topy btmy-topy btmy-midy))
      (when (zerop btmy-topy)
	(return-from draw-2d-filled-triangle nil))
      ;; vertices now sorted
      ;; now check error conditions to prevent / by 0
      ;; now fill top sub-triangle
      (let ((d0 (/ (float midx-topx) midy-topy))
	    (d1 (/ (float btmx-topx) btmy-topy))
	    (d2 (/ (float btmx-midx) btmy-midy))
	    (sx0 (float topx))
	    (sx1 (float topx)))
	(tagbody
	   (when (zerop midy-topy)
	     (go DRAW-LOWER-TRIANGLE))
	   DRAW-UPPER-TRIANGLE
	   (loop
	      for y from topy to midy
	      do
		(sdl2:render-draw-line renderer (truncate sx0) (truncate y) (truncate sx1) (truncate y))
		(incf sx0 d0)
		(incf sx1 d1))
	   (when (zerop btmy-midy)
	     (return-from draw-2d-filled-triangle nil))
	   DRAW-LOWER-TRIANGLE
	   (loop
	      for y from midy to btmy
	      do
		(sdl2:render-draw-line renderer (truncate sx0) (truncate y) (truncate sx1) (truncate y))
		(incf sx0 d2)
		(incf sx1 d1)))))))
  
  
  
(let ((coords (make-array 6 :element-type
			  '(signed-byte 32))))
  (defun draw-3d-triangle (tri renderer &key (filled t))
    (declare (type boolean filled))
    (loop for i below 3 do
	 (let* ((tmat (sb-cga:matrix* *pmat* *vmat* *rotmat*))
		(tv (sb-cga:transform-point 
		     (aref tri i)
		     tmat))
		(x (* (/ (1+ (aref tv 0)) 2.0) *x-res*))
		(y (* (/ (- 1 (aref tv 1)) 2.0) *y-res*)))
	   (setf (aref coords (* 2 i))       (round x)
		 (aref coords (1+ (* 2 i)))  (round y))))
    (if filled
	(draw-2d-filled-triangle (aref coords 0) (aref coords 1)
				 (aref coords 2) (aref coords 3)
				 (aref coords 4) (aref coords 5)
				 renderer)
	(progn
	  (sdl2:render-draw-line 
	   renderer
	   (aref coords 0) (aref coords 1)
	   (aref coords 2) (aref coords 3))
	  (sdl2:render-draw-line 
	   renderer
	   (aref coords 2) (aref coords 3)
	   (aref coords 4) (aref coords 5))
	  (sdl2:render-draw-line 
	   renderer
	   (aref coords 4) (aref coords 5)
	   (aref coords 0) (aref coords 1))))))
  
