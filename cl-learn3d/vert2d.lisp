(in-package #:cl-learn3d)

(defstruct vert2i
  (x 0 :type (signed-byte 16))
  (y 0 :type (signed-byte 16)))

(defun vert2swap (v1 v2)
  (let ((tx (vert2i-x v1))
	(ty (vert2i-y v1)))
    (setf (vert2i-x v1) (vert2i-x v2)
	  (vert2i-y v1) (vert2i-y v2)
	  (vert2i-x v2) tx
	  (vert2i-y v2) ty)
    nil))
