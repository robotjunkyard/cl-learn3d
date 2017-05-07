;;;; cl-learn3d.lisp

(in-package #:cl-learn3d)

(defparameter *model* nil)
(defparameter *delay* 1.0)
(defparameter *font* nil)
(defparameter *draw-frame* 0)

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

(defparameter *vmat*
  (look-at (sb-cga:normalize (sb-cga:vec 0.0 1.0 1.0))
	   (sb-cga:vec 0.0 0.0 0.0)
	   (sb-cga:vec 1.0 0.0 0.0))  "World's view matrix")
(defparameter *pmat*
  (ortho-projection 8.0 8.0 0.25 8.0) "World's projection matrix")
(defparameter *rotmat*
  (sb-cga:identity-matrix)            "World's rotation matrix")
(defparameter *world-matrix*
  (sb-cga:identity-matrix))
(declaim (type (simple-array single-float (16)) *vmat* *pmat* *rotmat* *world-matrix*))
(defparameter *axis-size* 4.0)

(defparameter *x-res* 640)
(defparameter *y-res* 480)
(declaim (type uint16 *x-res* *y-res*))
(defun setres (x y)
  (setf *x-res* x
	*y-res* y))
		
(defun rotate ()
  (setf *rotmat* 
	(axis-rotate (sb-cga:normalize (sb-cga:vec 1.0 0.0 0.0))
		     (mod (* 0.001 (sdl2:get-ticks)) 360.0)))
)

(defun render-stuff (renderer)
  (setq *world-matrix* (sb-cga:matrix* *pmat* *vmat* *rotmat*))
  (sdl2:set-render-draw-color renderer 0 0 0 255)
  (sdl2:render-clear renderer)
  (rotate)
  (sdl2:set-render-draw-color renderer 64 127 255 255)
  (when *model*
    (draw-axes renderer)
    ;;;; (sort-mesh-face-draw-order *model*)
    (sdl2:set-render-draw-color renderer 207 205 155 255)
    (draw-mesh *model* renderer))
  (sdl2:render-present renderer))

(defun main ()
  (sb-ext:gc :full t)
  (with-main
    (setq *vmat*
	  (look-at 
	   (sb-cga:normalize
	    (sb-cga:vec 0.0 1.0 1.0))
	   (sb-cga:vec 0.0 0.0 0.0)
	   (sb-cga:vec 1.0 0.0 0.0))

	  *model* (load-model "ico"))
    (sdl2:with-init (:everything)
      (sdl2:with-window (win :title "Learn3D" :flags '(:shown)
			     :w *x-res* :h *y-res*)
	(sdl2:with-renderer (renderer win :flags '(:accelerated :presentvsync))
	  (sdl2:with-event-loop (:method :poll)
	    (:idle
	     ()
	     (sleep (/ *delay* 60))
	     (continuable
	       ;;; (format t " ------ ~8d ------ ~%" *draw-frame*)
	       (render-stuff renderer)
	       (incf *draw-frame*))
	     #+SWANK (update-swank))
	    (:quit ()
		   t)))))))

