(in-package :cl-learn3d)

(defparameter *vmat*
  (sb-cga:identity-matrix)
  "World's view matrix; used to project vertices from world-space into view-space.")
(defparameter *pmat*
  (sb-cga:identity-matrix)
  "World's projection matrix; used to project vertices from view-space into clip-space.")
(defparameter *rotation-matrix*
  (sb-cga:identity-matrix)  "World's rotation matrix")
(defparameter *translation-matrix*
  (sb-cga:identity-matrix)  "World's translation matrix")
(defparameter *scale-matrix*
  (sb-cga:identity-matrix)  "World's scale matrix")

(defparameter *transformation-matrix*
  (sb-cga:identity-matrix)
  "World's transformation (Translation * Rotation * Scale) matrix.  Should be auto-calculated once right before *world-matrix* gets calculated.")
(defparameter *world-matrix*
  (sb-cga:identity-matrix)
  "This should be auto-calculated once every time right before drawing anything, according to the other matrices.")

(declaim (type mat4x4 *vmat* *pmat* *rotation-matrix* *translation-matrix* *scale-matrix* *transformation-matrix* *world-matrix*))

(defun update-world-transformation-matrix ()
  "Sets the world's *transformation-matrix* to be TranslationMatrix X RotationMatrix X ScaleMatrix."
  (setf *transformation-matrix*
	(sb-cga:matrix* *translation-matrix* *rotation-matrix* *scale-matrix*)))

(defun update-world-matrix ()
  (setf *world-matrix*
	(sb-cga:matrix* *pmat* *vmat* *transformation-matrix*)))

(defun translate (x y z)
  (declare (type single-float x y z))
  (sb-cga:matrix 1.0 0.0 0.0 x
		 0.0 1.0 0.0 y
		 0.0 0.0 1.0 z
		 0.0 0.0 0.0 1.0))

(defun rotate (angle ux uy uz)
  (declare (type single-float angle ux uy uz))
  (let* ((a (deg2rad angle))
	 (c (cos a))
	 (s (sin a))
	 (m11 (+ (* ux ux (- 1.0 c)) c))
	 (m12 (- (* ux uy (- 1.0 c)) (* uz s)))
	 (m13 (+ (* ux uz (- 1.0 c)) (* uy s)))
	 (m21 (+ (* uy ux (- 1.0 c)) (* uz s)))
	 (m22 (+ (* uy uy (- 1.0 c)) c))
	 (m23 (- (* uy uz (- 1.0 c)) (* ux s)))
	 (m31 (- (* ux uz (- 1.0 c)) (* uy s)))
	 (m32 (+ (* uy uz (- 1.0 c)) (* ux s)))
	 (m33 (+ (* uz uz (- 1.0 c)) c)))
    (declare (type single-float a c s m11 m12 m13 m21 m22 m23 m31 m32 m33))
    (sb-cga:matrix m11 m12 m13 0.0
		   m21 m22 m23 0.0
		   m31 m32 m33 0.0
		   0.0 0.0 0.0 1.0)
    ))

(defun scale (x y z)
  (declare (type single-float x y z))
  (sb-cga:matrix x 0.0 0.0 0.0
		 0.0 y 0.0 0.0
		 0.0 0.0 z 0.0
		 0.0 0.0 0.0 1.0))
		 
