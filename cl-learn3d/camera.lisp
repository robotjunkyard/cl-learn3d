(in-package :cl-learn3d)

;; T = W * V * P
;; T = final transform matrix that multiplies points to screen coords
;; W = World
;; V = View
;; P = Projection

(defvar *camera-eye* (sb-cga:vec 1.0 1.0 1.0)
  "For LOOK-AT to memorize the camera's position for the rest of the program to query if needed.")
(defvar *camera-target* (sb-cga:vec 0.0 0.0 0.0))
(declaim (type vec3 *camera-eye* *camera-target*))

(defun look-at (eye target up)
  "Returns a View matrix."
  (declare (type sb-cga:vec eye target up))
  (let* 
      ((vz (sb-cga:normalize (sb-cga:vec- eye target)))
       (vx (sb-cga:normalize (sb-cga:cross-product up vz)))
       (vy (sb-cga:cross-product vz vx))
       (dotxi- (- (sb-cga:dot-product vx eye)))
       (dotyi- (- (sb-cga:dot-product vy eye)))
       (dotzi- (- (sb-cga:dot-product vz eye)))
       (ivm 
	(sb-cga:matrix (aref vx 0)  (aref vy 0)  (aref vz 0)  0.0
		       (aref vx 1)  (aref vy 1)  (aref vz 1)  0.0
		       (aref vx 2)  (aref vy 2)  (aref vz 2)  0.0
		       dotxi-       dotyi-       dotzi-       1.0)))
    ivm))

(defun ortho-projection (width height near far)
  "Returns a Projection matrix."
  (declare (type single-float width height near far))
  (sb-cga:matrix 
   (/ 1.0 width)  0.0   0.0  0.0
   0.0  (/ 1.0 height)  0.0  0.0
   0.0  0.0  (/ -2.0 (- far near))  (- (/ (+ far near) (- far near)))
   0.0  0.0  0.0  1.0))

(defun perspective-projection (fov near far)
  (declare (type single-float fov near far))
  (let ((s (/ 1.0 (tan (* (/ fov 2.0) #.(/ +pi-s+ 180.0))))))
    (declare (type single-float s))
    (sb-cga:matrix
     s     0.0     0.0     0.0
     0.0   s       0.0     0.0
     0.0   0.0  (- (/ far (- far near)))   -1.0
     0.0   0.0  (- (/ (* far near) (- far near)))  0.0)))

(defun frustum-projection (top bottom left right near far)
  (declare (type single-float top bottom left right near far))
  (let ((m11 (/ (* 2.0 near) (- right left)))
	(m13 (/ (+ right left) (- right left)))
	(m22 (/ (* 2.0 near) (- top bottom)))
	(m23 (/ (+ top bottom) (- top bottom)))
	(m33 (- (/ (+ far near) (- far near))))
	(m34 (- (/ (* 2.0 near far) (- far near)))))
    (declare (type single-float m11 m13 m22 m23 m33 m34))
    (sb-cga:matrix
     m11  0.0  m13  0.0
     0.0  m22  m23  0.0
     0.0  0.0  m33  m34
     0.0  0.0 -1.0  0.0)))
