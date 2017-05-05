(in-package #:cl-learn3d)

(defun draw-triangle-2d (x1 y1 x2 y2 x3 y3)
  (let ((v1 (make-vert2i :x x1 :y y1))
	(v2 (make-vert2i :x x2 :y y2))
	(v3 (make-vert2i :x x3 :y y3))
	(y1i 0) 
	(y2i 0)
	(y3i 0)
    (when (< (vert2i-y v1) (vert2i-y) v2)
      (vert2swap v1 v2))
    (when (< (vert2i-y v1) (vert2i-y) v3)
      (vert2swap v1 v3))
    (when (< (vert2i-y v2) (vert2i-y) v3)
      (vert2swap v2 v3))
    (when (= y1i y3i)
      (return-from draw-triangle-2d nil))


(let ((coords (make-array 6 :element-type
			  '(signed-byte 32))))
  (defun draw-tri (tri &optional (renderer *renderer*))
    (loop for i below 3 do
	 (let* ((tmat (sb-cga:matrix* *pmat* *vmat* *rotmat*))
		(tv (sb-cga:transform-point 
		     (aref tri i)
		     tmat))
		(x (* (/ (1+ (aref tv 0)) 2.0) *x-res*))
		(y (* (/ (- 1 (aref tv 1)) 2.0) *y-res*)))
	   ;; (format t "~a --> ~a --> (~a, ~a)~%" 
	   ;;    (aref tri i) tv x y)
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
     (aref coords 0) (aref coords 1))
    (draw-axes renderer)))
