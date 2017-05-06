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
