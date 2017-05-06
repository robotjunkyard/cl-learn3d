;;;; cl-learn3d.lisp

(in-package #:cl-learn3d)

(defparameter *model* nil)
(defparameter *delay* 90.0)
(defparameter *font* nil)

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

(defstruct dbgvert
  x1 y1 x2 y2 x3 y3)
(defparameter *debug-tris* (make-array 4096 :adjustable t :fill-pointer 0))
(defparameter *show-dbg-tris* nil)
(defparameter *show-dbg-i* 0)
(defun render-stuff (renderer)
  (sdl2:set-render-draw-color renderer 0 0 0 255)
  (sdl2:render-clear renderer)
  (rotate)
  (sdl2:set-render-draw-color renderer 64 127 255 255)
  (if *show-dbg-tris*
      (progn
	(setq *show-dbg-i* (mod (1+ *show-dbg-i*) (length *show-dbg-tris*)))
	(with-slots (x1 y1 x2 y2 x3 y3)
	    (aref *show-dbg-tris* *show-dbg-i*)
	  (draw-2d-filled-triangle 
	   x1 y1 x2 y2 x3 y3
	   renderer
	   :PRINT-STATS t)))
      (progn
	(let ((x1 (random *x-res*))
	      (x2 (random *x-res*))
	      (x3 (random *x-res*))
	      (y1 (random *y-res*))
	      (y2 (random *y-res*))
	      (y3 (random *y-res*)))
	  (draw-2d-filled-triangle 
	   x1 y1 x2 y2 x3 y3
	   renderer
	   :PRINT-STATS nil)
	  ;; (format t "~d~%" (length *debug-tris*))
	  (unless *show-dbg-tris*
	    (vector-push-extend (make-dbgvert :x1 x1 :y1 y1 :x2 x2 :y2 y2 :x3 x3 :y3 y3) *debug-tris*)))))

  #|(when *model*
    (draw-axes renderer)
    (sdl2:set-render-draw-color renderer 207 205 155 255)
    (draw-mesh *model* renderer))|#
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
	     (sleep (/ *delay* 60))
	     (continuable
	       (render-stuff renderer))
	     #+SWANK (update-swank))
	    (:quit ()
		   t)))))))

