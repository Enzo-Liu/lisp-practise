;;; ch1.lisp ---
;;
;; Filename: ch1.lisp
;; Description:
;; Author: Liu Enze
;; Maintainer:
;; Created: Sat Nov 29 17:30:04 2014 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated: Sun Nov 30 12:40:06 2014 (+0800)
;;           By: Liu Enze
;;     Update #: 27
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;  For all the exercises in SICP Charpter1 .
;;  Using Common Lisp (SBCL v 1.2.2)
;;  Based on SICP 2nd version
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

;;Utility
(defun id (x) x)

;;Ex1

10 ;;10
(+ 5 3 4) ;;12
(- 9 1) ;;8
(/ 6 2) ;;3
(+ (* 2 4) (- 4 6)) ;;6
(defvar a 4) ;;a
(defvar b (+ a 1)) ;;b
(+ a b (* a b)) ;;29
(= a b) ;;nil
(if (and (> b a) (< b (* a b)))
    b
    a) ;;5
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (t 25)) ;;6
(+ 2 (if (> b a) b a)) ;;7
(* (cond ((> a b) a)
         ((< a b) b)
         (t -1))
   (+ a 1)) ;;25

;;Ex2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7))) ;;-37/150

;;Ex3
(defun sum-of-largest-two (a b c)
  (let ((min (min a b c)))
    (- (+ a b c) min)))

;;Ex4
(defun a-plus-abs-b (a b)
  "Return a plus b if b is positive, else return a minus b.
So, according to b, choose the fit function between + and -, then apply this
function to (a b). "
  (funcall (if (> b 0) #'+ #'-) a b))

;;Ex5
"1. 0 2. infinite loop"

;;Ex6
"Infinite loop. Since before calling new-if, all the params will be evaluted, so the sprt function will always be called.
"

;;Ex7
(defun my-sqrt (n)
  "don't work for 0..."
  (labels ((good-enough (guess last)
             (< (/ (abs  (- guess last)) last) 0.000001))
           (improve (guess)
             (/ (+ guess (/ n guess)) 2))
           (sqrt-iter (guess last)
             (if (good-enough guess last)
                 guess
                 (sqrt-iter (improve guess) guess))))
    (sqrt-iter 2 1)))

;;Ex8
(defun my-cube-root (n)
  "don't work for 0..."
  (labels ((good-enough (guess last)
             (< (/ (abs  (- guess last)) last) 0.000001))
           (improve (guess)
             (/ (+ (/ n (* guess guess )) (* 2 guess)) 3))
           (cube-iter (guess last)
             (if (good-enough guess last)
                 guess
                 (cube-iter (improve guess) guess))))
    (cube-iter 2 1)))

;;Ex9
(defun my-+-1 (a b)
  "recursive calculation. (+ 4 5) ==> (1+ (+ 3 5)) ==> (1+ (1+ (+ 2 5))) ..."
  (if (= a 0)
      b
      (1+ (+ (1- a) b))))

(defun my-+-2 (a b)
  "iterate calculation. (+ 4 5) ==> (+ 3 6) ==> (+ 2 7) ..."
  (if (= a 0)
      b
      (+ (1- a) (1+ b))))

;;Ex10
(defun A (x y)
  (cond ((= y 0) 0)
        ((= x 0 ) (* 2 y))
        ((= y 1 ) 2)
        (t (A (1- x) (A x (1- y))))))

(A 1 10) ;; 1024
(A 2 4) ;; 65536
(A 3 3) ;; 65536

(defun f (n) (A 0 n)) ; ==> f(n)=2n
(defun g (n) (A 1 n)) ; ==> g(n)=2^n (n>0);g(n)=0 (n=0); infinite loop (n<0)
(defun h (n) (A 2 n)) ; ==> h(n)=2^(2^n) (n>0);h(n)=0;infinite loop (n<0)

;;Ex11
(defun f-r (n)
  "recurise version of f"
  (if (< n 3)
      n
      (+ (f-r (- n 1)) (* 2 (f-r (- n 2))) (* 3 (f-r (- n 3))))))
(defun f-i (n)
  "iterate version of f"
  (labels
      ((func (a b c)
         (+ a (* 2 b) (* 3 c)))
       (f-iter (n a b c)
         (if (= n 0)
             (func a b c)
             (f-iter (1- n) (func a b c) a b))))
    (if (< n 3)
        n
        (f-iter (- n 3) 2 1 0))))

(dotimes (i 10) (format t "recu: ~d, iter: ~d~%" (f-r i) (f-i i)))

;;Ex 12
(defun tri (n m)
  "calculate the yanghui triangle, the n column and the m row."
  (cond ((= m 0) 1)
        ((= (1- n) m) 1)
        (t (+ (tri (1- n) (1- m)) (tri (1- n) m)))))
(dotimes (i 7) (dotimes (j i) (format t "~d " (tri i j))) (format t "~%"))

;;Ex13

;;Ex16
(defun my-exp (b n)
  "calculate b^n.  In exp-iter , a is a state"
  (labels ((exp-iter (a b n)
             (cond ((= n 0) a)
                   ((= (mod n 2) 0) (exp-iter a (* b b) (/ n 2)))
                   (t (exp-iter (* a b) (* b b) (/ (1- n) 2))))))
    (exp-iter 1 b n)))

;;Ex17 Ex18
(defun my-multi (b n)
  "calculate b*n. "
  (labels ((multi-iter (a b n)
             (cond ((= n 0) a)
                   ((= (mod n 2) 0) (multi-iter a (+ b b) (/ n 2)))
                   (t (multi-iter (+ a b) (+ b b) (/ (1- n) 2))))))
    (multi-iter 0 b n)))

;;Ex19
(defun my-fast-fib (n)
  "T is (p q | q p+q) (a b) is (0 1), T^2 makes p1=p^2+q^2,q1=pq+q(p+q)"
  (labels ((fib-iter (a b p q n)
             (cond ((= n 0) a)
                   ((evenp n) (fib-iter a b
                                        (+ (* p p) (* q q))
                                        (+ (* q q) (* 2 p q))
                                        (/ n 2)))
                   (t (fib-iter
                       (+ (* a p) (* b q))
                       (+ (* a q) (* b q) (* b p))
                       p q
                       (1- n) )))))
    (fib-iter 0 1 0 1 n)))
(dotimes (i 10) (print (my-fast-fib i)))

;;Ex21
(defun smallest-divisor (n)
  (loop for i from 2 to n
     when (= 0 (mod n i)) return i))

(mapcar #'smallest-divisor '(199 1999 19999)) ; ==> (199 1999 7)

;;Ex22,23
(defun next-odd (n) (if (evenp n ) (1+ n) (+ n 2)))
(defun smallest-divisor-1 (n)
  (do ((i 2 (next-odd i)))
      ((= 0 (mod n i)) i)))
(defun my-primep (n)
  (if (= (smallest-divisor-1 n) n) t nil))
(defun next-prime (n)
  (do ((i n (next-odd i)))
      ((my-primep i) i)))

(defun smallest-primes (start num)
  (do* ((sp (next-prime start) (next-prime (1+ sp)))
        (i 1 (1+ i))
        (res (list sp) (append res (list sp))))
       ((= i num) res)))

(time (smallest-primes 1000 3))
(time (smallest-primes 10000 3))
(time (smallest-primes 100000 3))

;;Ex24-28
;;...

;;Ex29
(defun simpson (fn n a b)
  (let ((h (/ (- b a) n)))
    (do* ((i 0 (1+ i))
          (y (funcall fn (+ a (* i h))) (funcall fn (+ a (* i h))))
          (sum y (+ sum (* y (cond ((= i n) 1)
                                   ((evenp (- i n)) 2)
                                   (t 4))))))
         ((= i n) (/ (* h sum) 3)))))
(defun my-cube (x) (* x x x))
(simpson #'my-cube 100 0 1) ;; ==> 1/4
(simpson #'my-cube 10000 0 1) ;; ==> 1/4

;;Ex30
(defun sum (term a next b)
  "Original recusive version."
  (if (> a b)
      0
      (+ (funcall term a)
         (sum term (funcall next a) next b))))
(defun my-sum (term a next b)
  "Iterate version."
  (labels ((sum-iter (cur res)
             (if (> cur b)
                 res
                 (sum-iter (funcall next cur) (+ (funcall term cur) res)))))
    (sum-iter a 0)))


;;Ex31
(defun my-product (term a next b)
  "Iterate version."
  (labels ((product-iter (cur res)
             (if (> cur b)
                 res
                 (product-iter (funcall next cur) (* (funcall term cur) res)))))
    (product-iter a 1)))

(defun my-product-1 (term a next b)
  "Recursive version."
  (if (> a b)
      1
      (* (funcall term a)
         (my-product-1 term (funcall next a) next b))))

(defun factorial (n)
  (my-product #'(lambda (x) x) 1 #'1+ n))

(factorial 8);; ==> 40320

(defun square (x) (* x x))
(defun cal-pi (n)
  (* (/ 8 (* 2 (1+ n)))
     (my-product #'(lambda (x) (square (/ (* 2 (1+ x)) (+ 1 (* 2 x)))))
                 1
                 #'1+
                 n)))

(float (cal-pi 1000)) ;; ==> 3.1423774

;;Ex32
(defun accumulate (combiner base term a next b)
  (labels ((iter (cur res)
             (if (> cur b)
                 res
                 (iter (funcall next cur)
                       (funcall combiner res (funcall term cur))))))
    (iter a base)))
(defun a-sum (term a next b) (accumulate #'+ 0 term a next b))
(defun a-product (term a next b) (accumulate #'* 1 term a next b))

(a-product #'(lambda (x) x) 1 #'1+ 10 );; ==>3628800

;;Ex33
(defun filter-accumulate (combiner filter base term a next b)
  (labels ((iter (cur res)
             (if (> cur b)
                 res
                 (iter (funcall next cur)
                       (if (funcall filter cur)
                           (funcall combiner res (funcall term cur))
                           res)))))
    (iter a base)))
(defun sum-prime (a b)
  (filter-accumulate #'+ #'my-primep 0 #'id a #'1+ b)) ;; (sum-prime 2 5) ==> 10
(defun product-relatively-prime-less-than-n (n)
  (filter-accumulate #'* #'(lambda (x) (= 1 (gcd x n))) 1 #'id 1 #'1+ n)) ;;(product-relatively-prime-less-than-n 5) ==> 24

;;Ex34
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (ex34-f #'ex34-f)
;; 2 fell through ETYPECAñSE expression.
;; Wanted one of (SYMBOL FUNCTION).
;;[Condition of type SB-KERNEL:CASE-FAILURE]
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ex34-f (g)
  (funcall g 2))

;;Ex35
(defparameter *tolerance* 0.0000001)
(defun fix-point (f guess)
  (labels ((closep (v1 v2)
             (< (abs (-  v1 v2)) *tolerance*))
           (try (guess)
             (let ((next (funcall f guess)))
               (if (closep next guess)
                   next
                   (try next)))))
    (try guess)))
(defun gold-radio-gen (x) (+ 1 (/ 1 x)))
(fix-point #'gold-radio-gen 1)

;;Ex36
(defun fix-point-print (f guess)
  (labels ((closep (v1 v2)
             (< (abs (-  v1 v2)) *tolerance*))
           (try (guess)
             (let ((next (funcall f guess)))
               (print guess)
               (if (closep next guess)
                   next
                   (try next)))))
    (try guess)))

;;Ex37-40 ...

;;Ex41
(defun my-double (f)
  (lambda (x) (funcall f (funcall f x))))

(funcall (funcall (my-double (my-double #'my-double)) #'1+) 5)

;;Ex42
(defun compose (f g)
  (lambda (x) (funcall f (funcall g x))))

(funcall (compose #'square #'1+) 6 ) ;;==> 49

;;Ex43
(defun repeated (f n)
  (if (= n 1)
      f
      (compose f (repeated f (1- n)))))

(funcall (repeated #'square 2) 5) ;==> 625

;;Ex44
(defparameter *dx* 0.0001)
(defun smooth (f)
  (lambda (x) (/ (+ (funcall f (+ x *dx*))
               (funcall f (- x *dx*))
               (funcall f x))
            3)))

(defun smooth-n (f n)
  (funcall (repeated #'smooth n) f))

;;Ex45-46...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ch1.lisp ends here
