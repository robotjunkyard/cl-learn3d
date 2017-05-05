(in-package :cl-learn3d)

;; T = W * V * P
;; T = final transform matrix that multiplies points to screen coords
;; W = World
;; V = View
;; P = Projection

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
		       dotxi-       dotyi-       dotzi-       1.0))
	;;(sb-cga:matrix (aref vx 0)  (aref vx 1)  (aref vx 2)  0.0
	;;	       (aref vy 0)  (aref vy 1)  (aref vy 2)  0.0
	;;	       (aref vz 0)  (aref vz 1)  (aref vz 2)  0.0
	;;	       (aref eye 0) (aref eye 1) (aref eye 2) 1.0)
	 )
    ivm))
	 ;;;(sb-cga:inverse-matrix ivm)


(defun ortho-projection (width height near far)
  "Returns a Projection matrix."
  (declare (type single-float width height near far))
  (sb-cga:matrix 
   (/ 1.0 width)  0.0   0.0  0.0
   0.0  (/ 1.0 height)  0.0  0.0
   0.0  0.0  (/ -2.0 (- far near))  (- (/ (+ far near) (- far near)))
   0.0  0.0  0.0  1.0))

(defun ortho (left right bottom top near far)
  (sb-cga:matrix
   (/ 2.0 (- right left))  0.0  0.0  (- (/ (+ right left)
					   (- right left)))
   0.0  (/ 2.0 (- top bottom))  0.0  (- (/ (+ top bottom)
					   (- top bottom)))
   0.0  0.0  (/ -2.0 (- far near))  (- (/ (+ far near)
					  (- far near)))
   0.0  0.0  0.0  1.0))

(defun axis-rotate (u a)
  (declare (type sb-cga:vec u)
	   (type single-float a)
	   (optimize (speed 3) (debug 0) (safety 0)))
  ;; most of these are for legibility
  (let* ((ux (aref u 0))
	 (uy (aref u 1))
	 (uz (aref u 2))
	 (ux^2 (* ux ux))
	 (uy^2 (* uy uy))
	 (uz^2 (* uz uz))
	 (cosa (cos a))       ;; ensure these don't needlessly
	 (sina (sin a))       ;; calculate more than needed
	 (omcosa (- 1 cosa))  ;; one minus cos angle
	 (r11 (+ cosa (* ux^2 omcosa)))
	 (r12 (- (* ux uy omcosa) (* uz sina)))
	 (r13 (+ (* ux uz omcosa) (* uy sina)))
	 (r21 (+ (* uy ux omcosa) (* uz sina)))
	 (r22 (+ cosa (* uy^2 omcosa)))
	 (r23 (- (* uy uz omcosa) (* ux sina)))
	 (r31 (- (* uz ux omcosa) (* uy sina)))
	 (r32 (+ (* uz uy omcosa) (* ux sina)))
	 (r33 (+ cosa (* uz^2 omcosa))))
    (declare (type single-float ux uy uz ux^2 uy^2 uz^2 cosa sina omcosa
		   r11 r12 r13 r21 r22 r23 r31 r32 r33))
    (sb-cga:matrix r11 r12 r13 0.0
		   r21 r22 r23 0.0
		   r31 r32 r33 0.0
		   0.0 0.0 0.0 1.0)))
