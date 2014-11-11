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

;;;chapter 5
(defun ascii (fn start end)
  (loop for index from start to end do
       (loop repeat (funcall fn index) do
            (format t "*"))
       (format t "~%")))

;;;chapter 8
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
          ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
                ,@body)))))
