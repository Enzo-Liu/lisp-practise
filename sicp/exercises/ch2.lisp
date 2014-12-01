;;; ch2.lisp ---
;;
;; Filename: ch2.lisp
;; Description:
;; Author: Liu Enze
;; Maintainer:
;; Created: Sun Nov 30 22:18:14 2014 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated: Sun Nov 30 23:22:07 2014 (+0800)
;;           By: Liu Enze
;;     Update #: 3
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
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ch2.lisp ends here
