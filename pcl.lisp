;;; FileName: practical common lisp

;;; chapter 3
(defvar *db* nil)

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun make-record (&key title artist rate)
  `(:title ,title :artist ,artist :rate ,rate))

(defun add-record (&key title artist rate)
  (push (make-record :title title :artist artist :rate rate) *db*))

(defun save-db (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax (setf *db* (read in)))))

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defmacro where (&rest clauses)
  (flet ((make-comparison (key value)
           `(equal (getf x ,key) ,value))
         (make-comparison-list (fields)
           (loop while fields collecting
                (make-comparison (pop fields) (pop fields)))))
    `#'(lambda (x) (and ,@(make-comparison-list clauses)))))
