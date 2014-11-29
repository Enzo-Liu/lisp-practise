;;; streams.lisp ---
;;
;; Filename: streams.lisp
;; Description:
;; Author: Liu Enze
;; Maintainer:
;; Created: Fri Nov 28 07:51:31 2014 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated: Fri Nov 28 07:51:31 2014 (+0800)
;;           By: Liu Enze
;;     Update #: 1
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


;;; streams learn from sicp
;;; translate into common lisp
;;; code begins

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

(defun filter-stream (fn stream)
  (if (funcall fn (car-stream stream))
      (cons-stream (car-stream stream)
                   (filter-stream fn (cdr-stream stream)))
      (filter-stream fn (cdr-stream stream))))

(defun stream-nullp (stream)
  (null stream))

(defun ref-stream (s n)
  (if (= n 0)
      (car-stream s)
      (ref-stream (cdr-stream s) (1- n))))

(defun print-stream (stream n)
  (if (<= n 0)
      nil
      (progn (print (car-stream stream))
             (print-stream (cdr-stream stream) (1- n)))))

(defparameter *fib*
  (cons-stream 0
               (cons-stream 1
                            (add-streams (cdr-stream *fib*)
                                         *fib*))))

(defun fibgen (a b)
  (cons-stream a (fibgen b (+ a b))))

(defparameter *nfib* (fibgen 0 1))

(defparameter *even-fib* (filter-stream #'(lambda (x) (evenp x)) *nfib*))

(defparameter *integer* (cons-stream 1 (map-stream #'(lambda (x) (1+ x)) *integer*)))

(defparameter *integer-2* (cons-stream 2 (map-stream #'(lambda (x) (1+ x)) *integer*)))

(defun sieve (stream)
  (cons-stream (car-stream stream)
               (sieve (filter-stream #'(lambda (x) (/= (mod x (car-stream stream)) 0))
                                     (cdr-stream stream)))))

(defparameter *prime* (sieve *integer-2*))

(defparameter *ones* (cons-stream 1 *ones*))

(defparameter *integer-3* (cons-stream 1 (add-streams *integer-3* *ones*)))

(defun nfib (n)
  (ref-stream *nfib* n))

(defun fib (n)
  (ref-stream *fib* n))

(defun even-fib (n)
  (ref-stream *even-fib* n))

(defun ei (low high)
  (if (> low high)
      +EMPTY-STREAM+
      (cons-stream
       low
       (ei (1+ low) high))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; streams.lisp ends here
