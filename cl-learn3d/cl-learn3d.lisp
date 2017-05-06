;;;; cl-learn3d.lisp

(in-package #:cl-learn3d)

(defparameter *model* nil)

(defmacro with-main (&body body)
  "Enables REPL access via UPDATE-SWANK in the main loop using SDL2. Wrap this around
the sdl2:with-init code."
  ;;TODO: understand this. Without this wrapping the sdl:with-init the sdl thread
  ;; is an "Anonymous thread" (tested using sb-thread:*current-thread*), while applying
  ;; this makes *current-thread* the same as the one one when queried directly from the
  ;; REPL thread: #<SB-THREAD:THREAD "repl-thread" RUNNING {adress...}>
  `(sdl2:make-this-thread-main
    (lambda ()
      ;; does work on linux+sbcl without the following line:
      #+sbcl (sb-int:with-float-traps-masked (:invalid) ,@body)
      #-sbcl ,@body)))

(defmacro continuable (&body body)
  `(restart-case
       (progn ,@body)
     (continue () :report "Continue")))

(defun update-swank ()
#-SWANK  nil
#+SWANK  (continuable
          (let ((connection (or swank::*emacs-connection*
                                (swank::default-connection))))
            (when connection
              (swank::handle-requests connection t)))))

(defparameter *tri-verts* 
  (make-array 3 :initial-contents
	      (list
	       (sb-cga:vec -4.0  -4.0  0.0)
	       (sb-cga:vec  4.0  -4.0  0.0)
	       (sb-cga:vec  0.0   4.0  0.0))))

(defparameter *vmat* 
  (look-at (sb-cga:normalize (sb-cga:vec 0.0 1.0 1.0))
	   (sb-cga:vec 0.0 0.0 0.0)
	   (sb-cga:vec 1.0 0.0 0.0)))
(defparameter *pmat*
  (ortho-projection 8.0 8.0 0.25 8.0))
(defparameter *rotmat*
  (sb-cga:identity-matrix))
(defparameter *axis-size* 4.0)

(defparameter *x-res* 640)
(defparameter *y-res* 480)
(defun setres (x y)
  (setf *x-res* x
	*y-res* y))

;;;; NOTE:
;;;; Reason using SDL to draw lines here and elsewhere, instead of writing custom
;;;; code that draws pixels direct to the buffer in Lisp itself, is because foreign function
;;;; calls and (I suspect, but not 100% certain) foreign memory alterations are not
;;;; a free lunch (performance-wise) in Lisp.  This probably means, for filled/textured
;;;; polygons later on, will write custom C-based API to do such 2D-ish operations and
;;;; glue it into here w/ Lisp bindings;  to be designed around doing as much as possible
;;;; with as few foreign calls as possible.
;;;;
;;;; If any Lisp experts can tell me if I'm full of crap or if I'm overlooking something
;;;; in my presumptions, please let me know :)
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
		
(defun rotate ()
  (setf *rotmat* 
	(axis-rotate (sb-cga:vec 0.0 0.0 1.0)
		     (mod (* 0.001 (sdl2:get-ticks)) 360.0))))

(defun render-stuff (renderer)
  (sdl2:set-render-draw-color renderer 0 0 0 255)
  (sdl2:render-clear renderer)
  (rotate)
  (draw-axes renderer)
  (sdl2:set-render-draw-color renderer 255 255 255 255)
  (draw-triangle *tri-verts* renderer)
  (when *model*
    (sdl2:set-render-draw-color renderer 207 205 155 255)
    (draw-mesh *model* renderer))
  (sdl2:render-present renderer))

(defun main ()
  (with-main
    (setq *vmat*
	  (look-at 
	   (sb-cga:normalize
	    (sb-cga:vec 0.0 1.0 1.0))
	   (sb-cga:vec 0.0 0.0 0.0)
	   (sb-cga:vec 1.0 0.0 0.0))

	  *model* (load-model "ship"))
    (sdl2:with-init (:everything)
      (sdl2:with-window (win :title "Learn3D" :flags '(:shown)
			     :w *x-res* :h *y-res*)
	
	(sdl2:with-renderer (renderer win :flags '(:accelerated :presentvsync))
	  (sdl2:with-event-loop (:method :poll)
	    (:idle
	     ()
	     (sleep (/ 1.0 60))
	     (continuable
	       (render-stuff renderer))
	     #+SWANK (update-swank))
	    (:quit () t)))))))

