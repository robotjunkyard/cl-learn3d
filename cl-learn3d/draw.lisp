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
    
(let ((coords (make-array 6 :element-type
			  '(signed-byte 32))))
  (defun draw-triangle (tri renderer)
    (loop for i below 3 do
	 (let* ((tmat (sb-cga:matrix* *pmat* *vmat* *rotmat*))
		(tv (sb-cga:transform-point 
		     (aref tri i)
		     tmat))
		(x (* (/ (1+ (aref tv 0)) 2.0) *x-res*))
		(y (* (/ (- 1 (aref tv 1)) 2.0) *y-res*)))
	   (setf (aref coords (* 2 i))       (round x)
		 (aref coords (1+ (* 2 i)))  (round y))))
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
     (aref coords 0) (aref coords 1))))
