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

(defun quicksort (arr lo hi &key (test #'<))
  "Sorts the integer elements of the array.  This is an in-place sort, thus alters the original contents of the passed array."
  (declare (type (simple-array * (*)) arr)
	   (type uint32 lo hi)
	   (type function test))
  (flet ((%partition (arr lo hi)
	   (declare (type (simple-array * (*)) arr)
		    (type uint32 lo hi))
	   (let ((pivot (svref arr lo))
		 (i (1- lo))
		 (j (1+ hi)))
	     (declare (type int32 pivot i j))
	     (loop do
		  (loop do
		       (incf i)
		     while (< (the fixnum (svref arr i)) pivot))
		  (loop do
		       (decf j)
		     while (> (the fixnum (svref arr j)) pivot))  
		  (when (>= i j)
		    (return-from %partition j))
		  (rotatef (svref arr i) (svref arr j))))))
    (when (funcall test lo hi)
      (let ((p (%partition arr lo hi)))
	(quicksort arr lo p)
	(quicksort arr (1+ p) hi))))
  arr)

