;;;; anything not really related to graphics or too small for its own file goes in here

(in-package :cl-learn3d)

(defconstant +pi-s+ (coerce pi 'single-float))
(declaim (type single-float +pi-s+))

(defun tokenize-text-file-into-lines (filename)
  (declare (type (or string pathname) filename))
  (with-open-file (in filename)
    (loop for line = (read-line in nil 'eof)
       until (eq line 'eof)
       collect line)))

;;; from a Kent Pitman usenet post, though I lost the URL
(defmacro case-using (pred-exp exp &body clauses)
  (let ((temp (gensym)) (pred (gensym "PRED")))
    `(let ((,pred ,pred-exp) (,temp ,exp))
       (cond ,@(mapcar #'(lambda (clause)
                           (destructuring-bind (keys . clause-forms) clause
                             (cond ((eq keys 'otherwise)
                                    `(t ,@clause-forms))
                                   (t
                                    (if (atom keys) (setq keys (list keys)))
                                    `((member ,temp ',keys :test ,pred)
                                      ,@clause-forms)))))
                       clauses)))))

(defun quicksort-iv (arr lo hi &key (valuator #'identity))
  "Sorts the numeric elements of the array.  This is an in-place sort, thus alters the original contents of the passed array.  This presumes the valuator returns an integer."
  (declare (type (simple-array uint32 (*)) arr)
	   (type uint32 lo hi)
	   (type function valuator))
  (flet ((%partition (arr p r)
	   (declare (type (simple-array uint32 (*)) arr)
		    (type uint32 p r))
	   (let ((x (funcall valuator (aref arr p)))
		 (i (1- p))
		 (j (1+ r)))
	     (declare (type fixnum x)
		      (type int32 i j))
	     (loop do
		  (loop do
		       (decf j)
		     until (<= (the fixnum (funcall valuator (aref arr j))) (the fixnum x)))
		  (loop do
		       (incf i)
		     until (>= (the fixnum (funcall valuator (aref arr i))) (the fixnum x)))
		  (if (< (the int32 i) (the int32 j))
		      (rotatef (aref arr i) (aref arr j))
		      (return-from %partition (the int32 j)))))))
    (when (< lo hi)
      (let ((p (%partition arr lo hi)))
	(quicksort-iv arr lo p :valuator valuator)
	(quicksort-iv arr (1+ p) hi :valuator valuator))))
  arr)


(defun quicksort-fv (arr lo hi &key (valuator #'identity))
  "Sorts the numeric elements of the array.  This is an in-place sort, thus alters the original contents of the passed array.  This presumes the valuator returns a single-float."
  (declare (type (simple-array uint32 (*)) arr)
	   (type uint32 lo hi)
	   (type function valuator))
  (flet ((%partition (arr p r)
	   (declare (type (simple-array uint32 (*)) arr)
		    (type uint32 p r))
	   (let ((x (funcall valuator (aref arr p)))
		 (i (1- p))
		 (j (1+ r)))
	     (declare (type single-float x)
		      (type int32 i j))
	     (loop do
		  (loop do
		       (decf j)
		     until (<= (the single-float (funcall valuator (aref arr j))) (the single-float x)))
		  (loop do
		       (incf i)
		     until (>= (the single-float (funcall valuator (aref arr i))) (the single-float x)))
		  (if (< (the int32 i) (the int32 j))
		      (rotatef (aref arr i) (aref arr j))
		      (return-from %partition (the int32 j)))))))
    (when (< lo hi)
      (let ((p (%partition arr lo hi)))
	(quicksort-fv arr lo p :valuator valuator)
	(quicksort-fv arr (1+ p) hi :valuator valuator))))
  arr)



;; yanked from my game project, to profile function performance
(defun is-lambda? (fun)
  (declare (type function fun))
  (multiple-value-bind (lam      ;; function's defining lambda expression
                        clo      ;; enclosed in non-null lexical environment?
                        name)    ;; what we care about
      (function-lambda-expression fun)
    (consp name)))

;; yanked from my game project, to profile function performance
(defun get-all-symbols (&optional package-name)
  "Get all function-bound symbols in a package."
  (let ((lst nil)
        (package (find-package package-name)))
    (do-all-symbols (s lst)
      (when (and (fboundp s)
                 (not (null (symbol-function s)))
                 (not (is-lambda? (symbol-function s))))
        (if package
            (when (eql (symbol-package s) package)
              (push s lst))
            (push s lst))))
    lst))

;; yanked from my game project, to profile function performance
(defun profile-all (&optional (package-name (package-name *package*)))
  (eval
   (append (list 'sb-profile:profile)
           (loop for sym in (get-all-symbols package-name) collect sym))))

(declaim (inline deg2rad))
(defun deg2rad (degrees)
  (declare (type single-float degrees))
  (/ (* degrees +pi-s+) 180.0))

(defmacro clamp (val low high)
  ;; (declare (type (or integer single-float) val low high))
  `(min (max ,val ,low) ,high))


(defstruct colorf
  (r 0.0 :type single-float)
  (g 0.0 :type single-float)
  (b 0.0 :type single-float)
  (a 1.0 :type single-float))

(defstruct colori
  (r 0   :type uint8)
  (g 0   :type uint8)
  (b 0   :type uint8)
  (a 255 :type uint8))

(defun convert-colorf-to-colori (colorf)
  (declare (type colorf colorf))
  (make-colori :r (clamp (truncate (* 255 (colorf-r colorf))) 0 255)
	       :g (clamp (truncate (* 255 (colorf-g colorf))) 0 255)
	       :b (clamp (truncate (* 255 (colorf-b colorf))) 0 255)
	       :a (clamp (truncate (* 255 (colorf-a colorf))) 0 255)))
