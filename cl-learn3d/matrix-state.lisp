(in-package :cl-learn3d)

(defparameter *vmat*
  (sb-cga:identity-matrix)
  #|(look-at (sb-cga:normalize (sb-cga:vec 0.0 1.0 1.0))
	   (sb-cga:vec 0.0 0.0 0.0)
	   (sb-cga:vec 1.0 0.0 0.0))|#
  "World's view matrix; used to project vertices from world-space into view-space.")
(defparameter *pmat*
  (sb-cga:identity-matrix)
  ;;;(perspective-projection 90.0 0.5 40.0)
  ;;;(ortho-projection 8.0 8.0 0.25 8.0)
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

(defun rotate (r ux uy uz)
  (declare (type single-float r ux uy uz))
  (let ((rx (deg2rad ux))
	(ry (deg2rad uy))
	(rz (deg2rad uz)))
    (axis-rotate (sb-cga:vec rx ry rz) r)))

(defun scale (x y z)
  (declare (type single-float x y z))
  (sb-cga:matrix x 0.0 0.0 0.0
		 0.0 y 0.0 0.0
		 0.0 0.0 z 0.0
		 0.0 0.0 0.0 1.0))
		 
