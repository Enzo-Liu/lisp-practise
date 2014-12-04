;;; ch2.lisp ---
;;
;; Filename: ch2.lisp
;; Description:
;; Author: Liu Enze
;; Maintainer:
;; Created: Sun Nov 30 22:18:14 2014 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated: Wed Dec  3 10:29:39 2014 (+0800)
;;           By: Liu Enze
;;     Update #: 11
;; URL:
;; Doc URL:
;; Keywords: sicp exercise chapter2
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

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.1. Define a better version of make-rat that handles both positive
;; and negative arguments. Make-rat should normalize the sign so that if the
;; rational number is positive, both the numerator and denominator are positive,
;; and if the rational number is negative, only the numerator is negative.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-rat (n d)
  (let ((g (gcd n d)))
    (if (< d 0)
        (cons (/ (- n) g) (/ (- d) g))
        (cons (/ n g) (/ d g)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.2. Consider the problem of representing line segments in a
;; plane. Each segment is represented as a pair of points: a starting point and
;; an ending point. Define a constructor make-segment and selectors
;; start-segment and end-segment that define the representation of segments in
;; terms of points. Furthermore, a point can be represented as a pair of
;; numbers: the x coordinate and the y coordinate. Accordingly, specify a
;; constructor make-point and selectors x-point and y-point that define this
;; representation. Finally, using your selectors and constructors, define a
;; procedure midpoint-segment that takes a line segment as argument and returns
;; its midpoint (the point whose coordinates are the average of the coordinates
;; of the endpoints). To try your procedures, you'll need a way to print points:
;; (define (print-point p)
;; (newline)
;; (display "(")
;; (display (x-point p))
;; (display ",")
;; (display (y-point p))
;; (display ")")
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-point (x y)
  (cons x y))
(defun x-point (point)
  (car point))
(defun y-point (point)
  (cdr point))

(defun make-segment (start end)
  (cons start end))
(defun start-segment (segment)
  (car segment))
(defun end-segment (segment)
  (cdr segment))
(defun mid-in-two-points (p1 p2)
  (make-point (/ (+ (x-point p1) (x-point p2)) 2)
              (/ (+ (y-point p1) (y-point p2)) 2)))
(defun mid-point (segment)
  (mid-in-two-points (start-segment segment)
                     (end-segment segment)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.3. Implement a representation for rectangles in a plane. (Hint:
;; You may want to make use of exercise 2.2.) In terms of your constructors and
;; selectors, create procedures that compute the perimeter and the area of a
;; given rectangle. Now implement a different representation for rectangles. Can
;; you design your system with suitable abstraction barriers, so that the same
;; perimeter and area procedures will work using either representation?
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-rectangle (start end)
  (cons start end))
(defun length-rectangle (rectangle)
  (abs (- (x-point (car rectangle)) (x-point (cdr rectangle)))))
(defun height-rectangle (rectangle)
  (abs (- (y-point (car rectangle)) (y-point (cdr rectangle)))))

(defun make-rectangle-2 (start length high)
  (list start length high))
(defun length-rectangle-2 (rectangle)
  (nth 1 rectangle))
(defun height-rectangle-2 (rectangle)
  (nth 2 rectangle))

(defun perimeter (rectangle)
  (* 2 (+ (length-rectangle rectangle)
          (height-rectangle rectangle))))
(defun area (rectangle)
  (* (length-rectangle rectangle)
     (height-rectangle rectangle)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.4. Here is an alternative procedural representation of pairs. For
;; this representation, verify that (car (cons x y)) yields x for any objects x
;; and y.
;; (define (cons x y)
;; (lambda (m) (m x y)))
;; (define (car z)
;; (z (lambda(p q) p)))
;; What is the corresponding definition of cdr? (Hint: To verify that
;; this works,make use of the substitution model of section 1.1.5.)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-cons (x y)
  (lambda (m) (funcall m x y)))
(defun my-car (z)
  (funcall z #'(lambda (p q) p)))
(defun my-cdr (z)
  (funcall z #'(lambda (p q) q)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.5. Show that we can represent pairs of nonnegative integers using
;; only numbers and arithmetic operations if we represent the pair a and b as
;; the integer that is the product 2^a 3^b. Give the corresponding definitions
;; of the procedures cons, car, and cdr.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-cons-integer (x y)
  (* (expt 2 x) (expt 3 y)))
(defun my-car-integer (z)
  (if (/= 0 (mod z 2))
      0
      (1+ (my-car-integer (/ z 2)))))
(defun my-cdr-integer (z)
  (if (/= 0 (mod z 3))
      0
      (1+ (my-cdr-integer (/ z 3)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.6. In case representing pairs as procedures wasn't mind-boggling
;; enough, consider that, in a language that can manipulate procedures, we can
;; get by without numbers (at least insofar as nonnegative integers are
;; concerned) by implementing 0 and the operation of adding 1 as (define zero
;; (lambda (f) (lambda (x) x))) (define (add-1 n) (lambda (f) (lambda (x) (f ((n
;; f) x))))) This representation is known as Church numerals, after its
;; inventor, Alonzo Church, the logician who invented the lambda calculus.
;; Define one and two directly (not in terms of zero and add-1). (Hint: Use
;; substitution to evaluate (add-1 zero)). Give a direct definition of the
;; addition procedure + (not in terms of repeated application of add-1).
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *zero* (lambda (f) (lambda (x) x)))
(defun add-1 (n)
  (lambda (f) (lambda (x) (funcall f (funcall (funcall n f) x)))))
(defvar *one* (lambda (f) (lambda (x) (funcall f x))))
(defun add (a b)
  (lambda (f) (lambda (x)
           (funcall (funcall a f) (funcall (funcall b f) x)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.7. Alyssa's program is incomplete because she has not specified
;; the implementation of the interval abstraction. Here is a definition of the
;; interval constructor: (define (make-interval a b) (cons a b)) Define
;; selectors upper-bound and lower-bound to complete the implementation.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-interval (a b) (cons a b))
(defun lower-bound (z) (car z))
(defun upper-bound (z) (cdr z))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.8. Using reasoning analogous to Alyssa's, describe how the
;; difference of two intervals may be computed. Define a corresponding
;; subtraction procedure, called sub-interval.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sub-interval (x y)
  (make-interval (- (upper-bound x) (lower-bound y))
                 (- (lower-bound x) (upper-bound y))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Exercise 2.9. The width of an interval is half of the difference between its
;;upper and lower bounds. The width is a measure of the uncertainty of the
;;number specified by the interval. For some arithmetic operations the width of
;;the result of combining two intervals is a function only of the widths of the
;;argument intervals, whereas for others the width of the combination is not a
;;function of the widths of the argument intervals. Show that the width of the
;;sum (or difference) of two intervals is a function only of the widths of the
;;intervals being added (or subtracted). Give examples to show that this is not
;;true for multiplication or division.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun width (x)
  (/ (- (upper-bound x) (lower-bound x)) 2))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (width (add-interval x y))
;; (width (make-inverval (+ (lower-bound x) (lower-bound y))
;;                       (+ (upper-bound x) (upper-bound y))
;; (/ (-  (+ (upper-bound x) (upper-bound y))
;;        (+ (lower-bound x) (lower-bound y))) 2)
;; (+ (width x) (width y))
;; muliply example:
;; (mul-interval (1 . 2) (-2 . -1)) ==>  (-1 . -4) ;; width is 3
;; (* (width (1 . 2)) (width (-2 . -1))) ==> 1
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.10. Ben Bitdiddle, an expert systems programmer, looks over
;; Alyssa's shoulder and comments that it is not clear what it means to divide
;; by an interval that spans zero. Modify Alyssa's code to check for this
;; condition and to signal an error if it occurs.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun div-interval (x y)
  (let ((uy (upper-bound y))
        (ly (lower-bound y)))
    (cond ((<= (* ly uy) 0) error "zero is between the bounds of y")
          (t  (mul-interval x (make-interval ly uy))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.11. In passing, Ben also cryptically comments: ``By testing the
;; signs of the endpoints of the intervals, it is possible to break mul-interval
;; into nine cases, only one of which requires more than two multiplications.''
;; Rewrite this procedure using Ben's suggestion.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mul-interval (x y)
  (let ((ux upper-bound x)
        (uy upper-bound y)
        (lx lower-bound x)
        (ly lower-bound y))
    (cond
      ;; [-2,-1] [-2,-1]
      ((and (<= ux 0) (<= uy 0))
       (make-interval (* ux uy) (* lx ly)))
      ;; [-2,-1] [1,2]
      ((and (<= ux 0) (>= ly 0))
       (make-interval (* lx uy) (* ux ly)))
      ;; [-2,-1] [-1,2]
      ((and (<= ux 0) (<= ly 0) (>= uy 0))
       (make-interval (* lx uy) (* lx ly)))
      ;; [1,2] [1,2]
      ((and (>= lx 0) (>= ly 0))
       (make-interval (* lx ly) (* ux uy)))
      ;; [1,2] [-2,-1]
      ((and (>= lx 0) (<= uy 0))
       (make-interval (* ux ly) (* lx uy)))
      ;; [1,2] [-1,1]
      ((and (>= lx 0) (<= ly 0) (>= uy 0))
       (make-interval (* ux ly) (* ux uy)))
      ;; [-1,1] [1,2]
      ((and (<= lx 0) (>= ux 0) (>= ly 0))
       (make-interval (* lx uy) (* ux uy)))
      ;; [-1,1] [-2,-1]
      ((and (<= lx 0) (>= ux 0) (<= uy 0))
       (make-interval (* ux ly) (* lx ly)))
      ;; [-1,1] [-1,1]
      ((and (<= lx 0) (>= ux 0) (<= ly 0) (>= uy 0))
       (let ((p1 (* lx ly)) (p2 (* lx uy))
             (p3 (* ux uy)) (p4 (* ux ly)))
         (make-interval
          (min p1 p2 p3 p4)
          (max p1 p2 p3 p4))))
      (t error "not possible!"))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.12. Define a constructor make-center-percent that takes a center
;; and a percentage tolerance and produces the desired interval. You must also
;; define a selector percent that produces the percentage tolerance for a given
;; interval. The center selector is the same as the one shown above.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-center-percent (c p)
  (make-interval (- c (* c p)) (+ c (* c p))))
(defun center-interval (x)
  (/ (+ (lower-bound x) (upper-bound x)) 2))
(defun percent-interval (x)
  (/ (width x) (center-interval x)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.13. Show that under the assumption of small percentage tolerances
;; there is a simple formula for the approximate percentage tolerance of the
;; product of two intervals in terms of the tolerances of the factors. You may
;; simplify the problem by assuming that all numbers are positive.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; product of precents of two intervals
(* (percent-interval x) (percent-interval y))
;; ==>
(/ (* (width x) (width y)) (* (center-interval x) (center-interval y)))
;; if x y all positive , the percent of product of two intervals is  ==>
(percent-interval (make-interval (* (lower-bound x)
                                    (lower-bound y))
                                 (* (- (* 2 (center-interval x)) (lower-bound x))
                                    (- (* 2 (center-interval y)) (lower-bound y)))))
;; ==>


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.14. Demonstrate that Lem is right. Investigate the behavior of the
;; system on a variety of arithmetic expressions. Make some intervals A and B,
;; and use them in computing the expressions A/A and A/B. You will get the most
;; insight by using intervals whose width is a small percentage of the center
;; value. Examine the results of the computation in center-percent form (see
;; exercise 2.12).
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.15. Eva Lu Ator, another user, has also noticed the different
;; intervals computed by different but algebraically equivalent expressions. She
;; says that a formula to compute with intervals using Alyssa's system will
;; produce tighter error bounds if it can be written in such a form that no
;; variable that represents an uncertain number is repeated. Thus, she says,
;; par2 is a ``better'' program for parallel resistances than par1. Is she
;; right? Why?
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.16. Explain, in general, why equivalent algebraic expressions may
;; lead to different answers. Can you devise an interval-arithmetic package that
;; does not have this shortcoming, or is this task impossible? (Warning: This
;; problem is very difficult.)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ch2.lisp ends here
