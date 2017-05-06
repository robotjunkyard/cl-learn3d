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

		
(defun rotate ()
  (setf *rotmat* 
	(axis-rotate (sb-cga:vec 0.0 0.0 1.0)
		     (mod (* 0.001 (sdl2:get-ticks)) 360.0))))

(defun render-stuff (renderer)
  (sdl2:set-render-draw-color renderer 0 0 0 255)
  (sdl2:render-clear renderer)
  (rotate)
  (sdl2:set-render-draw-color renderer 64 127 255 255)
  (let* ((tick (sdl2:get-ticks))    ;; fix later
	 (rx (truncate (* 30.0 (sin (* 0.005 tick)))))
	 (ry (truncate (* 30.0 (cos (* 0.005 tick))))))
    (draw-2d-filled-triangle (- (truncate *x-res* 2) rx)
			     (+ ry 30)
			     
			     (+ rx 30)
			     (+ ry (- *y-res* 60))
			     
			     (+ rx (- *x-res* 60))
			     (+ ry (- *y-res* 100))
			     renderer))
  (when *model*
    (draw-axes renderer)
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

