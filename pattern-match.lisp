;;; pattern-match.lisp ---
;;
;; Filename: pattern-match.lisp
;; Description:
;; Author: Liu Enze
;; Maintainer:
;; Created: Fri Nov 28 07:50:20 2014 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated: Fri Nov 28 07:50:20 2014 (+0800)
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
;; this program is for sicp lecture 4b
;; it describes a common method to do pattern matching and calculate
;; lang common lisp
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



(defconstant +FAILED+ 'failed)

(defun atomp (x)
  (and (not (consp x))
       (not (null x))))

(defvar *init-dict* nil)

(defun variable-name (pattern)
  (cadr pattern))

(defun extend-dict (pattern exp dict)
  (let* ((name (variable-name pattern))
         (v (assoc name dict)))
    (cond ((not v) (cons (list name exp) dict))
          ((eq (cadr v) exp) dict)
          (t +Failed+))))

(equal (extend-dict '(? x) '10 *init-dict*)     '((x 10)))
(equal (extend-dict '(? x) '10 '((x 10)))      '((x 10)))
(equal (extend-dict '(? x) '10 '((x 11)))      +FAILED+)

(defun lookup (var dict)
  (let ((v (assoc var dict)))
    (if (not v)
        var
        (cadr v))))

(equal (assoc 'x '((x 10)))         '(x 10))
(equal (assoc 'x '((y 10) (x 10)))  '(x 10))
(equal (assoc 'z '((y 10) (x 10)))   nil)


(equal (lookup 'x '((x 10)))       10)
(equal (lookup 'y '((x 10)))       'y)

(defun arbitrary-constant-p (pattern)
  (and (consp pattern) (eq (car pattern) '?c)))

(defun constant-p (pattern)
  (numberp pattern))

(defun arbitrary-variable-p (pattern)
  (and (consp pattern) (eq (car pattern) '?v)))

(defun variable-p (pattern)
  (symbolp pattern))

(defun arbitrary-expression-p (pattern)
  (and (consp pattern) (eq (car pattern) '?)))

(defun match (pattern exp dict)
  (cond
    ((and (null pattern) (null exp)) dict)
    ((eq dict +FAILED+) +FAILED+)
    ((atomp pattern)
     (if (atomp exp)
         (if (eq pattern exp)
             dict
             +FAILED+)))
    ((arbitrary-constant-p pattern)
     (if (constantp exp)
         (extend-dict pattern exp dict)
         +FAILED+))
    ((arbitrary-variable-p pattern)
     (if (variable-p exp)
         (extend-dict pattern exp dict)
         +FAILED+))
    ((arbitrary-expression-p pattern)
     (extend-dict pattern exp dict))
    ((atomp exp) +FAILED+)
    (t (match (cdr pattern) (cdr exp)
              (match (car pattern) (car exp) dict)))))

(equal (match nil nil *init-dict*)                         *init-dict*)
(equal (match nil nil '((x 10)))                         '((x 10)))
(equal (match '* '* *init-dict*)                             *init-dict*)

(equal (match '(? x) '10 *init-dict*)                       '((x 10)))
(equal (match '(?c x) '10 *init-dict*)                      '((x 10)))
(equal (match '(?v x) '10 *init-dict*)                       +FAILED+)
(equal (match '(?v x) 'a *init-dict*)                       '((x a)))

(equal (match '((? x) (? x)) '(10 10) *init-dict*)          '((x 10)))
(equal (match '((? x) (? x)) '(10 11) *init-dict*)           +FAILED+)
(equal (match '((? x) (? y)) '(10 11) *init-dict*)          '((y 11) (x 10)))

(defvar *mult-deriv-pattern* '(+ (* (? x) (? y)) (? y)))

(equal (match *mult-deriv-pattern* '(+ (* 3 x) x) nil)      '((y x) (x 3)))
(equal (match *mult-deriv-pattern* '(+ (* 3 x) 4) nil)       +FAILED+)

(defun skeleton-evaluation-p (exp)
  (and (consp exp) (eq (car exp) '%)))

(defun eval-exp (skeleton)
  (cadr skeleton))


(defun evaluate (form dict)
  (if (atomp form)
      (lookup form dict)
      (apply (symbol-function (lookup (car form) dict) )
             (mapcar (lambda (v) (lookup v dict)) (cdr form)))))

(equal (evaluate '(+ x x) '((x 3))) 6)

(defun instantiate (skeleton dict)
  (labels ((run (s) (cond
                      ((null s) nil)
                      ((atomp s) s)
                      ((skeleton-evaluation-p s)
                       (evaluate (eval-exp s) dict))
                      (t (cons (run (car s)) (run (cdr s)))))))
    (run skeleton)))

(equal (instantiate 'foo *init-dict*)           'foo)
(equal (instantiate '(f a b) *init-dict*)       '(f a b))


(defvar *dict1* (match *mult-deriv-pattern* '(+ (* 3 x) x) nil))

(equal (instantiate '(% y) *dict1*)              'x)
(equal (instantiate '(% x) *dict1*)              '3)
(equal (instantiate '(+ (% x) (% y)) *dict1*)    '(+ 3 x))
(equal (instantiate '(% (+ x x)) *dict1*)        6)

(defun pattern (rule)
  (car rule))
(defun skeleton (rule)
  (cadr rule))

(defun simplifier (the-rules)
  (labels ((try-rules (exp rules)
             (if (null rules)
                 exp
                 (let ((dict (match (pattern (car rules))
                               exp
                               *init-dict*)))
                   (if (eq dict +FAILED+)
                       (try-rules exp (cdr rules))
                       (simplify-exp
                        (instantiate
                         (skeleton (car rules)) dict))))))
           (simplify-exp (exp)
             (try-rules (if (consp exp)
                            (mapcar #'simplify-exp exp)
                            exp)
                        the-rules)))
    #'simplify-exp))

(defparameter *deriv-rules*
  '(
    ((dd (?c c) (? v))             0)
    ((dd (?v v) (? v))             1)
    ((dd (?v u) (? v))             0)
    ((dd (+ (? x1) (? x2)) (? v))  (+ (dd (% x1) (% v))
                                    (dd (% x2) (% v))))
    ((dd (* (? x1) (? x2)) (? v))  (+ (* (% x1) (dd (% x2) (% v)))
                                    (* (dd (% x1) (% v)) (% x2))))
    ((dd (** (? x) (?c n)) (? v))  (* (* (% n) (+ (% x) (% (- n 1))))
                                    (dd (% x) (% v))))
    ))


(defparameter *algebra-rules*
  '(
    (((? op) (?c c1) (?c c2))                (% (op c1 c2)))
    (((? op) (?  e ) (?c c))                 ((% op) (% c) (% e)))
    ((+ 0 (? e))                             (% e))
    ((* 1 (? e))                             (% e))
    ((* 0 (? e))                             0)
    ((* (?c c1) (* (?c c2) (? e )))          (* (% (* c1 c2)) (% e)))
    ((* (?  e1) (* (?c c ) (? e2)))          (* (% c ) (* (% e1) (% e2))))
    ((* (* (? e1) (? e2)) (? e3))            (* (% e1) (* (% e2) (% e3))))
    ((+ (?c c1) (+ (?c c2) (? e )))          (+ (% (+ c1 c2)) (% e)))
    ((+ (?  e1) (+ (?c c ) (? e2)))          (+ (% c ) (+ (% e1) (% e2))))
    ((+ (+ (? e1) (? e2)) (? e3))            (+ (% e1) (+ (% e2) (% e3))))
    ((+ (* (?c c1) (? e)) (* (?c c2) (? e))) (* (% (+ c1 c2)) (% e)))
    ((* (? e1) (+ (? e2) (? e3)))            (+ (* (% e1) (% e2))))
    ))

(defun dsimp (exp) (funcall (simplifier *deriv-rules*) exp))
(defun alsimp (exp) (funcall (simplifier *algebra-rules*) exp))

(defun deriv (exp)
  (alsimp (dsimp exp)))

(equal (deriv '(dd (+ x y) x))          1)
(equal (deriv '(dd (* x y) x))         'y)
(equal (deriv '(dd (+ (* x y) x) x))   '(+ 1 y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pattern-match.lisp ends here
