(in-package :cl-learn3d)

(declaim (optimize (speed 3) (safety 1)))

(defparameter *default-triangle-color*
  (make-colori :r 128 :g 160 :b 160))

(defun draw-3d-line (x1 y1 z1 x2 y2 z2 tmat renderer)
  (declare (type single-float x1 y1 z1 x2 y2 z2)
	   (type mat4x4 tmat))
  (let* ((p1 (sb-cga:vec x1 y1 z1))
	 (p2 (sb-cga:vec x2 y2 z2))
	 (tp1 (sb-cga:transform-point p1 tmat))
	 (tp2 (sb-cga:transform-point p2 tmat))
	 (sx1 (round (* (1+ (aref tp1 0))  *x-res* 0.5)))
	 (sy1 (round (* (- 1 (aref tp1 1)) *y-res* 0.5)))
	 (sx2 (round (* (1+ (aref tp2 0))  *x-res* 0.5)))
	 (sy2 (round (* (- 1 (aref tp2 1)) *y-res* 0.5))))
    (declare (type vec3 p1 p2 tp1 tp2)
	     (dynamic-extent p1 p2 tp1 tp2)
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
  (declare (type int32 x1 y1 x2 y2 x3 y3)
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
    (declare (type int32 topx topy midx midy btmx btmy))
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
      (declare (type int32 midx-topx btmx-topx btmx-midx midy-topy btmy-topy btmy-midy))
      (when (zerop btmy-topy)
	(when print-stats
	  (format t " Fail A~%"))
	(return-from draw-2d-filled-triangle nil))
      ;; vertices now sorted
      ;; now check error conditions to prevent / by 0
      ;; now fill top sub-triangle
      (let ((dlong  (/ (float btmx-topx) (float btmy-topy)))
	    (dupper (/ (float midx-topx) (float midy-topy)))
	    (dlower (/ (float btmx-midx) (float btmy-midy)))
	    (sx0 (float topx))
	    (sx1 (float topx)))
	(declare (type single-float dlong dupper dlower sx0 sx1))
	(when print-stats
	  (format t "  D[~6a, ~6a, ~6a]~%" dlong dupper dlower))
	(tagbody
	   (unless fill
	     (go DRAW-WIREFRAME))
	   (when (zerop midy-topy)
	     (setq sx0 (float midx))
	     (when print-stats
	       (format t " Skip Upper~%"))
	     (go DRAW-LOWER-TRIANGLE))
	   DRAW-UPPER-TRIANGLE
	   (loop
	      for y from topy below midy
	      do
		(sdl2:render-draw-line renderer
				       (clamp (the int32 (round sx0)) 0 *x-res*)
				       (clamp y 0 *y-res*)
				       (clamp (the int32 (round sx1)) 0 *x-res*)
				       (clamp y 0 *y-res*))
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
		(sdl2:render-draw-line renderer
				       (clamp (the int32 (round sx0)) 0 *x-res*)
				       (clamp y 0 *y-res*)
				       (clamp (the int32 (round sx1)) 0 *x-res*)
				       (clamp y 0 *y-res*))
		(incf sx0 dlower)
		(incf sx1 dlong))
	 DRAW-WIREFRAME
	   (when wireframe
	     (sdl2:set-render-draw-color renderer 255 255 255 255)
	     (sdl2:render-draw-line renderer x1 y1 x2 y2)
	     (sdl2:render-draw-line renderer x2 y2 x3 y3)
	     (sdl2:render-draw-line renderer x3 y3 x1 y1)
	   )))))
  nil)


(defun calculate-tri-normal (v1 v2 v3)
  (declare (type vec3 v1 v2 v3)
	   (dynamic-extent v1 v2 v3))
  (let ((u (sb-cga:vec- v2 v1))
	(v (sb-cga:vec- v3 v1)))
    (declare (type vec3 u v)
	     (dynamic-extent u v))
    (sb-cga:cross-product u v)))
   
(defun draw-3d-triangle-* (x1 y1 z1 x2 y2 z2 x3 y3 z3 tmat
			   renderer &key (fill t) (wireframe t)
				      (color *default-triangle-color*)
				      (cull-backfaces t))
  (declare (type single-float x1 y1 x2 y2 x3 y3)
	   (type mat4x4 tmat)
	   (type boolean fill wireframe cull-backfaces)
	   (type colori color)
	   (dynamic-extent tmat))
  (let* ((v1  (sb-cga:vec x1 y1 z1))
	 (v2  (sb-cga:vec x2 y2 z2))
	 (v3  (sb-cga:vec x3 y3 z3))
	 (tv1 (sb-cga:transform-point v1 tmat))
	 (tv2 (sb-cga:transform-point v2 tmat))
	 (tv3 (sb-cga:transform-point v3 tmat)) )
    (declare (type vec3 v1 v2 v3 tv1 tv2 tv3)
	     (dynamic-extent v1 v2 v3 tv1 tv2 tv3))
    (let* ((normal (calculate-tri-normal tv1 tv2 tv3))
	   (cv (sb-cga:vec- *camera-target* *camera-eye*)))
	(when (and cull-backfaces (> (sb-cga:dot-product cv normal) 0))
	  (return-from draw-3d-triangle-* nil))
      (let* ((tv1x (aref tv1 0))
	     (tv1y (aref tv1 1))
	     (tv2x (aref tv2 0))
	     (tv2y (aref tv2 1))
	     (tv3x (aref tv3 0))
	     (tv3y (aref tv3 1))
	     (sx1 (truncate (* (+ 1.0 tv1x) *x-res-float*  0.5)))
	     (sy1 (round (* (- 1.0 tv1y) *y-res-float*  0.5)))
	     (sx2 (round (* (+ 1.0 tv2x) *x-res-float*  0.5)))
	     (sy2 (round (* (- 1.0 tv2y) *y-res-float*  0.5)))
	     (sx3 (round (* (+ 1.0 tv3x) *x-res-float*  0.5)))
	     (sy3 (round (* (- 1.0 tv3y) *y-res-float*  0.5))))
	(declare (type single-float tv1x tv1y tv2x tv2y tv3x tv3y)
		 (type fixnum sx1 sy1 sx2 sy2 sx3 sy3))
	#|(sdl2:set-render-draw-color renderer (colori-r color)
				    (colori-g color) (colori-b color)
				    (colori-a color))|#
	;; TODO: check if outside view frustum and don't bother drawing if so
	(draw-2d-filled-triangle sx1 sy1
				 sx2 sy2
				 sx3 sy3
				 renderer
				 :wireframe wireframe :fill fill
				 :color color
				 )))))
  
