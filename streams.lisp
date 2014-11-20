
(defconstant +EMPTY-STREAM+ nil)

(defmacro cons-stream (x y)
  `(cons ,x (delay ,y)))

(defun memo (fn)
  (let ((runp nil) (result nil))
    (lambda ()
      (if (not runp)
          (progn (setf result (funcall fn)) (setf runp t) result)
          result))))

(defmacro delay (exp)
  `(memo (lambda () ,exp)))

(defun car-stream (s)
  (car s))

(defun cdr-stream (s)
  (funcall (cdr s)))

(defun map-stream (fn &rest streams)
  (cons-stream (apply fn (mapcar #'car-stream streams))
               (apply 'map-stream `(,fn ,@(mapcar #'cdr-stream streams)))))

(defun add-streams (s1 s2)
  (map-stream #'+ s1 s2))

(defparameter *fib*
  (cons-stream 0
               (cons-stream 1
                            (add-streams (cdr-stream *fib*)
                                         *fib*))))
(defun ref-stream (s n)
  (if (= n 0)
      (car-stream s)
      (ref-stream (cdr-stream s) (1- n))))

(defun fib (n)
  (ref-stream *fib* n))

(defun ei (low high)
  (if (> low high)
      +EMPTY-STREAM+
      (cons-stream
       low
       (ei (1+ low) high))))
