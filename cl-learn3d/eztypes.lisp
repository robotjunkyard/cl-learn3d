(in-package :cl-learn3d)

(deftype int8 ()
  '(signed-byte 8))
(deftype uint8 ()
  '(unsigned-byte 8))
(deftype int16 ()
  '(signed-byte 16))
(deftype uint16 ()
  '(unsigned-byte 16))
(deftype int32 ()
  '(signed-byte 32))
(deftype uint32 ()
  '(unsigned-byte 32))

(deftype mat4x4 ()
  '(simple-array single-float (16)))
(deftype vec3 ()
  '(simple-array single-float (3)))
(deftype vec4 ()
  '(simple-array single-float (4)))

;;(defstruct colorf
;;  (r 0.0 :type single-float)
;;  (g 0.0 :type single-float)
;;  (b 0.0 :type single-float)
;;  (a 1.0 :type single-float))

(defstruct colori
  (r 0   :type uint8)
  (g 0   :type uint8)
  (b 0   :type uint8)
  (a 255 :type uint8))
