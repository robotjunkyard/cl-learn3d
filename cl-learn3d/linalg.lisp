(in-package #:cl-learn3d)

(deftype vec4 ()
  '(simple-array single-float (4)))
(deftype mat4 ()
  '(simple-array single-float (4 4)))

(declaim (inline vec4-p
		 mat4-p
		 v-x v-y v-z v-w
		 v-x-setf v-y-setf v-z-setf v-w-setf))
(defun vec4-p (v)
  (typep v 'vec4))
(defun mat4-p (m)
  (typep m 'mat4))
(defun v-x (v)
  (declare (type vec4 v))
  (aref v 0))
(defun v-y (v)
  (declare (type vec4 v))
  (aref v 1))
(defun v-z (v)
  (declare (type vec4 v))
  (aref v 2))
(defun v-w (v)
  (declare (type vec4 v))
  (aref v 3))
(defun v-x-setf (v val)
  (declare (type vec4 v)
	   (type single-float val))
  (setf (aref v 0) val))
(defun v-y-setf (v val)
  (declare (type vec4 v)
	   (type single-float val))
  (setf (aref v 1) val))
(defun v-z-setf (v val)
  (declare (type vec4 v)
	   (type single-float val))
  (setf (aref v 2) val))
(defun v-w-setf (v val)
  (declare (type vec4 v)
	   (type single-float val))
  (setf (aref v 3) val))
(defsetf v-x v-x-setf)
(defsetf v-y v-y-setf)
(defsetf v-z v-z-setf)
(defsetf v-w v-w-setf)

(defun make-vector (x y z &optional (w 0.0))
  (declare (type single-float x y z w))
  (the vec4
       (make-array 4 :element-type 'single-float
		   :initial-contents `(,x ,y ,z ,w))))

(defun make-matrix (v1x v1y v1z v1w
		    v2x v2y v2z v2w
		    v3x v3y v3z v3w
		    v4x v4y v4z v4w)
  (declare (type single-float 
		 v1x v1y v1z v1w
		 v2x v2y v2z v2w
		 v3x v3y v3z v3w
		 v4x v4y v4z v4w))
  (let ((init (list `(,v1x ,v1y ,v1z ,v1w)
		    `(,v2x ,v2y ,v2z ,v2w)
		    `(,v3x ,v3y ,v3z ,v3w)
		    `(,v4x ,v4y ,v4z ,v4w))))
    (declare (dynamic-extent init))
    (make-array '(4 4) :element-type 'single-float
		:initial-contents init)))

(declaim (inline vmag))
(defun vmag (v)
  (declare (type vec4 v))
  (let ((x (aref v 0))
	(y (aref v 1))
	(z (aref v 2))
	(w (aref v 3)))
    (declare (type single-float x y z w))
    (sqrt (+ (* x x) (* y y) (* z z) (* w w)))))

(defun vnormalize (v)
  (declare (type vec4 v))
  (let ((x (aref v 0))
	(y (aref v 1))
	(z (aref v 2))
	(w (aref v 3))
	(m (vmag v)))
    (declare (type single-float x y z w m))
    (if (= 0.0 m)
	v
	(make-vector (/ x m) (/ y m) (/ z m) (/ w m)))))

(defun v*v (u v)
  "Return the Cross Product of vectors u and v.  Note that this, naturally, ignores the W element since these are presumed to be three-component vectors."
  (declare (type vec4 u v))
  (let ((u1 (aref u 0))
	(u2 (aref u 1))
	(u3 (aref u 2))
	(v1 (aref v 0))
	(v2 (aref v 1))
	(v3 (aref v 2)))
    (make-vector (- (* u2 v3) (* u3 v2))
		 (- (* u3 v1) (* u1 v3))
		 (- (* u1 v2) (* u2 v1)))))

(defun v.v-3 (u v)
  "Return the Dot Product of the two vectors U and V.  This function ignores the W element."
  (declare (type vec4 u v))
  (+ (* (aref u 0) (aref v 0))
     (* (aref u 1) (aref v 1))
     (* (aref u 2) (aref v 2))))

(defun v.v (u v)
  "Return the Dot Product of the two vectors U and V."
  (declare (type vec4 u v))
  (the single-float 
       (+ (* (aref u 0) (aref v 0))
	  (* (aref u 1) (aref v 1))
	  (* (aref u 2) (aref v 2))
	  (* (aref u 3) (aref v 3)))))

;;  | a b c d | | Q |   | Qa + Rb + Sc + Td |
;;  | e f g h | | R | = | Qe + Rf + Sg + Th |
;;  | i j k l | | S |   | Qi + Rj + Sk + Tl |
;;  | m n o p | | T |   | Qm + Rn + So + Tp |
(defun m*v (m v)
  (declare (type mat4 m)
	   (type vec4 v))
  (let
      ((a (aref m 0 0)) (b (aref m 0 1)) (c (aref m 0 2)) (d (aref m 0 3))
       (e (aref m 1 0)) (f (aref m 1 1)) (g (aref m 1 2)) (h (aref m 1 3))
       (i (aref m 2 0)) (j (aref m 2 1)) (k (aref m 2 2)) (l (aref m 2 3))
       (mm (aref m 3 0)) (n (aref m 3 1)) (o (aref m 3 2)) (p (aref m 3 3))
       (vQ (aref v 0))
       (vR (aref v 1))
       (vS (aref v 2))
       (vT (aref v 3)))
    (declare (type single-float a b c d e f g h i j k l mm n o p 
		   vQ vR vS vT))
    (make-vector (+ (* vQ a) (* vR b) (* vS c) (* vT d))
		 (+ (* vQ e) (* vR f) (* vS g) (* vT h))
		 (+ (* vQ i) (* vR j) (* vS k) (* vT l))
		 (+ (* vQ mm) (* vR n) (* vS o) (* vT p)))))
		       
