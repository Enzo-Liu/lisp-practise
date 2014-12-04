;;; ch5.lisp ---
;;
;; Filename: ch5.lisp
;; Description:
;; Author: Liu Enze
;; Maintainer:
;; Created: Tue Dec  2 19:33:22 2014 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated: Tue Dec  2 20:00:16 2014 (+0800)
;;           By: Liu Enze
;;     Update #: 3
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

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 5.1. Design a register machine to compute factorials using the
;; iterative algorithm specified by the following procedure. Draw data-path and
;; controller diagrams for this machine.
;; (define (factorial n)
;;   (define (iter product counter)
;;     (if (> counter n)
;;       product
;;       (iter (* counter product) (+ counter 1)))
;;   (iter 1 1)))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(controller
 fac-iter
 (test (op >) (reg n) (const n))
 (branch (label fac-done))
 (assign b (op *) (reg b) (reg n))
 (assign n (op +) (reg n) (const 1))
 (goto (label fac-iter))
 fac-done
 ((op print) (reg b)))

(define fact-machine
    (make-machine
     '(c p n)
     (list (list '* *) (list '+ +) (list '> >))
     '((assign c (const 1))
       (assign p (const 1))
       test-n
       (test (op >) (reg c) (reg n))
       (branch (label fact-done))
       (assign p (op *) (reg c) (reg p))
       (assign c (op +) (reg c) (const 1))
       (goto (label test-n))
       fact-done)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ch5.lisp ends here
