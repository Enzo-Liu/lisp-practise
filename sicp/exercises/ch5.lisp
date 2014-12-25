;;; ch5.lisp ---
;;
;; Filename: ch5.lisp
;; Description:
;; Author: Liu Enze
;; Maintainer:
;; Created: Tue Dec  2 19:33:22 2014 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated: Thu Dec 25 22:15:02 2014 (+0800)
;;           By: 王 玉
;;     Update #: 53
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

;; (define fact-machine
;;     (make-machine
;;      '(c p n)
;;      (list (list '* *) (list '+ +) (list '> >))
;;      '((assign c (const 1))
;;        (assign p (const 1))
;;        test-n
;;        (test (op >) (reg c) (reg n))
;;        (branch (label fact-done))
;;        (assign p (op *) (reg c) (reg p))
;;        (assign c (op +) (reg c) (const 1))
;;        (goto (label test-n))
;;        fact-done)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 5.3. Design a machine to compute square roots using Newton's method,
;; as described in section 1.1.7:
;;
;; (define (sqrt x)
;;     (define (good-enough? guess)
;;         (< (abs (- (square guess) x)) 0.001))
;;   (define (improve guess)
;;       (average guess (/ x guess)))
;;   (define (sqrt-iter guess)
;;       (if (good-enough? guess)
;;           guess
;;           (sqrt-iter (improve guess))))
;;   (sqrt-iter 1.0))
;;
;; Begin by assuming that good-enough? and improve operations are available as
;; primitives. Then show how to expand these in terms of arithmetic
;; operations. Describe each version of the sqrt machine design by drawing a
;; data-path diagram and writing a controller definition in the register-machine
;; language.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun square-roots-machine ()
  (make-machine
   '(x g)
   (controller
    test-guess
    (test (op #'good-enough) (reg g) (reg x))
    (branch (label done))
    (assign g ((op #'improve) (reg g) (reg x)))
    (goto test-guess)
    done)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 5.4. Specify register machines that implement each of the following
;; procedures. For each machine, write a controller instruction sequence and
;; draw a diagram showing the data paths.  a. Recursive exponentiation:
;;
;; (define (expt b n)
;;     (if (= n 0)
;;         1(* b (expt b (- n 1)))))
;;
;; b. Iterative exponentiation:
;;
;; (define (expt b n)
;;     (define (expt-iter counter product)
;;         (if (= counter 0)
;;             product
;;             (expt-iter (- counter 1) (* b product))))
;;   (expt-iter n 1))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun expt-machine ()
  (make-machine
   '(b n val continue)
   `((= ,#'=) (- ,#'-) (* ,#'*))
   '(controller
     (assign continue (label done))
     expt-loop
     (test (op =) (reg n) (const 0))
     (branch (label answer))
     (save continue)
     (assign continue (label after-expt-n-1))
     (save n)
     (assign n (op -) (reg n) (const 1))
     (goto (label expt-loop))
     after-expt-n-1
     (restore n)
     (restore continue)
     (assign val (op *) (reg val) (reg b))
     (goto (reg continue))
     answer
     (assign val (const 1))
     (goto (reg continue))
     done)))

(defun expt-iter-machine ()
  (make-machine
   '(b n val)
   (controller
    (assign val (const 1))
    expt-iter
    (test (op #'=) (reg n) (const 0))
    (branch (label done))
    (assign val (op #'*) (reg val) (reg b))
    (assign n (op #'-) (reg n) (const 1))
    (goto (label expt-iter))
    done)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 5.5. Hand-simulate the factorial and Fibonacci machines, using some
;; nontrivial input (requiring execution of at least one recursive call). Show
;; the contents of the stack at each significant point in the execution.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; expt 2 5
;; step 1 (start):
;; => b : 2 n :5 continue: done val: nil  stack: []
;; step 2 (start loop):
;; => val: nil n : 4 b:2 continue: expt-after-n-1
;;    stack:[5,done]
;; step 3 (loop-again):
;; => val: nil n : 3 b:2 continue: expt-after-n-1
;;    stack:[4,after,5,done]
;; step 4 (loop-again):
;; => val: nil n : 2 b:2 continue: expt-after-n-1
;;    stack:[3,after,4,after,5,done]
;; step 5 (loop-again):
;; => val: nil n : 1 b:2 continue: expt-after-n-1
;;    stack:[2,after,3,after,4,after,5,done]
;; step 6 (loop-again):
;; => val: nil n : 0 b:2 continue: expt-after-n-1
;;    stack:[1,after,2,after,3,after,4,after,5,done]
;; step 7 (test-fit,goto answer):
;; => val: 1 n : 0 b:2
;;    stack:[1,after,2,after,3,after,4,after,5,done]
;; step 8 (expt-after-n-1):
;; => val: 2 n : 0 b:2
;;    stack:[2,after,3,after,4,after,5,done]
;; step 9 (expt-after-n-1):
;; => val: 4 n : 0 b:2
;;    stack:[3,after,4,after,5,done]
;; step 10 (expt-after-n-1):
;; => val: 8 n : 0 b:2
;;    stack:[4,after,5,done]
;; step 11 (expt-after-n-1):
;; => val: 16 n : 0 b:2
;;    stack:[5,done]
;; step 12 (expt-after-n-1):
;; => val: 32 n : 0 b:2
;;    stack:[]
;; step 13 (done):
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; expt-iter 2 5
;; step 1 (start):
;; => b : 2 n :5 val:1  stack: []
;; step 2 (start iter):
;; => val: 2 n : 4 b:2 stack:[]
;; step 3 (iter-again):
;; => val: 4 n : 3 b:2 stack:[]
;; step 4 (iter-again):
;; => val: 8 n : 2 b:2 stack:[]
;; step 5 (iter-again):
;; => val: 16 n : 1 b:2 stack:[]
;; step 6 (iter-again):
;; => val: 32 n : 0 b:2 stack:[]
;; step 7 (done):
;; => val: 32 n : 0 b:2 stack:[]
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 5.6. Ben Bitdiddle observes that the Fibonacci machine's controller
;; sequence has an extra save and an extra restore, which can be removed to make
;; a faster machine. Where are these instructions? ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(controller
 (assign continue (label fib-done))
 fib-loop
 (test (op <) (reg n) (const 2))
 (branch (label immediate-answer))
 (save continue)
 (assign continue (label afterfib-n-1))
 (save n)
 (assign n (op -) (reg n) (const 1))
 (goto (label fib-loop))
 afterfib-n-1
 (restore continue) ;; this can be removed
 (assign n (op -) (reg n) (const 2))
 (save continue) ;; this can be removed
 (assign continue (label afterfib-n-2))
 (save val)
 (goto (label fib-loop))
 afterfib-n-2
 (assign n (reg val))
 (restore val)
 (restore continue)
 (assign val
         (op +) (reg val) (reg n))
 (goto (reg continue))
 immediate-answer
 (assign val (reg n))
 (goto (reg continue))
 fib-done)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 5.7. Use the simulator to test the machines you designed in exercise
;; 5.4.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; first ,write the simulator  (in ./machine.lisp)
(load "./machine.lisp")
(defparameter *m* (expt-machine))
(set-register-contents *m* 'b 2)
;;DONE
(set-register-contents *m* 'n 2)
;;DONE
(start *m*)
;;DONE
(get-register-contents *m* 'val)
;;4

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 5.8.  The following register-machine code is ambiguous, because the
;; label here is defined more than once:
;; start
;; (goto (label here))
;; here
;; (assign a (const 3))
;; (goto (label there))
;; here
;; (assign a (const 4))
;; (goto (label there))
;; there
;; With the simulator as written, what will the contents of register a be when
;; control reaches there? Modify the extract-labels procedure so that the
;; assembler will signal an error if the same label name is used to indicate two
;; different locations.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ANSWER:
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; start-->here(the first one,because assoc only matches the first 1) -->
;; there
;; so a is 3
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter test-machine
  (make-machine
   '(a)
   '()
   '(start
     (goto (label here))
     here
     (assign a (const 3))
     (goto (label there))
     here
     (assign a (const 4))
     (goto (label there))
     there)))

(start test-machine)
;;DONE
(get-register-contents test-machine 'a)
;; 3


(defun extract-labels (text receive)
  (if (null text)
      (funcall receive '() '())
      (extract-labels (cdr text)
                      (lambda (insts labels)
                        (let ((next-inst (car text)))
                          (if (symbolp next-inst)
                              (funcall receive insts
                                       (cons (make-label-entry
                                              next-inst insts labels)
                                             labels))
                              (funcall receive (cons
                                                (make-instruction
                                                 next-inst)
                                                insts)
                                       labels)))))))

(defun make-label-entry (label-name insts labels)
  (when (assoc label-name labels)
    (error "Duplicate label definition: ~S" label-name))
  (cons label-name insts))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 5.9.  The treatment of machine operations above permits them to
;; operate on labels as well as on constants and the contents of
;; registers. Modify the expression-processing procedures to enforce the
;; condition that operations can be used only with registers and constants.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 5.10.  Design a new syntax for register-machine instructions and
;; modify the simulator to use your new syntax. Can you implement your new
;; syntax without changing any part of the simulator except the syntax
;; procedures in this section?
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 5.11.  When we introduced save and restore in section 5.1.4, we
;; didn't specify what would happen if you tried to restore a register that was
;; not the last one saved, as in the sequence

;; (save y)
;; (save x)
;; (restore y)

;; There are several reasonable possibilities for the meaning of restore:

;; a.  (restore y) puts into y the last value saved on the stack, regardless of
;; what register that value came from. This is the way our simulator
;; behaves. Show how to take advantage of this behavior to eliminate one
;; instruction from the Fibonacci machine of section 5.1.4 (figure 5.12).

;; b.  (restore y) puts into y the last value saved on the stack, but only if
;; that value was saved from y; otherwise, it signals an error. Modify the
;; simulator to behave this way. You will have to change save to put the
;; register name on the stack along with the value.

;; c.  (restore y) puts into y the last value saved from y regardless of what
;; other registers were saved after y and not restored. Modify the simulator to
;; behave this way. You will have to associate a separate stack with each
;; register. You should make the initialize-stack operation initialize all the
;; register stacks.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 5.12.  The simulator can be used to help determine the data paths
;; required for implementing a machine with a given controller. Extend the
;; assembler to store the following information in the machine model:

;; a list of all instructions, with duplicates removed, sorted by instruction
;; type (assign, goto, and so on);

;; a list (without duplicates) of the registers used to hold entry points (these
;; are the registers referenced by goto instructions);

;; a list (without duplicates) of the registers that are saved or restored;

;; for each register, a list (without duplicates) of the sources from which it
;; is assigned (for example, the sources for register val in the factorial
;; machine of figure 5.11 are (const 1) and ((op *) (reg n) (reg val))).

;; Extend the message-passing interface to the machine to provide access to this
;; new information. To test your analyzer, define the Fibonacci machine from
;; figure 5.12 and examine the lists you constructed.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 5.13.  Modify the simulator so that it uses the controller sequence
;; to determine what registers the machine has rather than requiring a list of
;; registers as an argument to make-machine. Instead of pre-allocating the
;; registers in make-machine, you can allocate them one at a time when they are
;; first seen during assembly of the instructions.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ch5.lisp ends here
