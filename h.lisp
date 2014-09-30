(ql:quickload 'qt)

(defpackage :hello-qt
  (:use :cl :qt)
  (:export #:main))

(in-package :hello-qt)
(named-readtables:in-readtable :qt)

;;;;Hello Qt

(defun hello-qt()
    (let* ((app (make-qapplication))
           (label (#_new QLabel "<strong>Hello Qt!</strong>")))
        (#_setWindowTitle label "Hello, Qt!")
        (#_resize label 100 100)
        (#_show label)
        (unwind-protect
            (#_exec app)
        (#_hide label))))
(hello-qt)
