;;;; anything not really related to graphics or too small for its own file goes in here

(in-package :cl-learn3d)

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

(defun quicksort (arr lo hi &key (valuator #'identity))
  "Sorts the numeric elements of the array.  This is an in-place sort, thus alters the original contents of the passed array."
  (declare (type (simple-array * (*)) arr)
	   (type uint32 lo hi)
	   (type function valuator))
  (flet ((%partition (arr p r)
	   (declare (type (simple-array * (*)) arr)
		    (type uint32 p r))
	   (let ((x (funcall valuator (aref arr p)))
		 (i (1- p))
		 (j (1+ r)))
	     (loop do
		  (loop do
		       (decf j)
		     until (<= (funcall valuator (aref arr j)) x))
		  (loop do
		       (incf i)
		     until (>= (funcall valuator (aref arr i)) x))
		  (if (< i j)
		      (rotatef (aref arr i) (aref arr j))
		      (return-from %partition j))))))
    (when (< lo hi)
      (let ((p (%partition arr lo hi)))
	(quicksort arr lo p :valuator valuator)
	(quicksort arr (1+ p) hi :valuator valuator))))
  arr)















(defun quicksort-* (arr lo hi &key (valuator #'identity))
  (declare (type (simple-array * (*)) arr)
	   (type fixnum lo hi)
	   (type function valuator))
  (flet ((%partition (arr p r)
	   (declare (type (simple-array * (*)) arr)
		    (type fixnum p r))
	   (let ((x (funcall valuator (aref arr p)))
		 (i (1- p))
		 (j (1+ r)))
	     (loop do
		  (loop do
		       (decf j)
		     until (<= (funcall valuator (aref arr j)) x))
		  (loop do
		       (incf i)
		     until (>= (funcall valuator (aref arr i)) x))
		  (if (< i j)
		      (rotatef (aref arr i) (aref arr j))
		      (return-from %partition j))))))
    (when (< lo hi)
      (let ((p (%partition arr lo hi)))
	(quicksort arr lo p)
	(quicksort arr (1+ p) hi))))
  arr)

