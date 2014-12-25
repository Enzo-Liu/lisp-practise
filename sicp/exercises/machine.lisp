;;; machine.lisp ---
;;
;; Filename: machine.lisp
;; Description:
;; Author: Liu Enze
;; Maintainer:
;; Created: Thu Dec 11 14:39:16 2014 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated: Thu Dec 25 22:15:08 2014 (+0800)
;;           By: 王 玉
;;     Update #: 43
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

(defun make-machine (registers ops controller-text)
  (let ((machine (make-new-machine)))
    (mapcar #'(lambda (register-name)
                (funcall
                 (funcall machine 'allocate-register) register-name))
            registers)
    (funcall (funcall machine 'install-operations) ops)
    (funcall (funcall machine 'install-instruction-sequence)
             (e_assemble controller-text machine))
    machine))

(defun make-register (name)
  (let ((contents '*unassigned*))
    (lambda (message)
      (cond ((eq message 'get) contents)
            ((eq message 'set)
             (lambda (value) (setf contents value)))
            (t (error "~S :Unknown request -- REGISTER ~S" message name))))))

(defun get-contents (register)
  (funcall register 'get))

(defun set-contents (register value)
  (funcall (funcall register 'set) value))

(defun make-stack ()
  (let ((s '()))
    (flet ((push-stack (x) (setf s (cons x s)))
           (pop-stack ()
             (if (null s)
                 (error "Empty stack --- POP")
                 (let ((top (car s)))
                   (setf s (cdr s))
                   top)))
           (initialize () (setf s '()) 'done))
      (lambda (message)
        (cond ((eq message 'push) #'push-stack)
              ((eq message 'pop) (pop-stack))
              ((eq message 'initialize) (initialize))
              (t (error "~S :Unknown request --STACK" message)))))))

(defun pop-stack (stack)
  (funcall stack 'pop))

(defun push-stack (stack value)
  (funcall  (funcall stack 'push) value))

(defun instruction-execution-proc (inst)
  (cdr inst))

(defun make-new-machine ()
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (funcall stack 'initialize)))))
          (register-table
           (list (cons 'pc pc) (cons 'flag flag))))
      (labels ((allocate-register (name)
                 (if (assoc name register-table)
                     (error "Multiply defined register: ~S" name)
                     (setf register-table
                           (cons (cons name (make-register name))
                                 register-table)))
                 'register-allocated)
               (lookup-register (name)
                 (let ((val (assoc name register-table)))
                   (if val (cdr val) (error "Unknown register: ~S~%~S"
                                            name
                                            register-table))))
               (execute ()
                 (let ((insts (get-contents pc)))
                   (if (null insts)
                       'done
                       (progn
                         (funcall (instruction-execution-proc (car insts)))
                         (execute))))))
        (lambda (message)
          (cond
            ((eq message 'instructions) the-instruction-sequence)
            ((eq message 'start)
             (set-contents pc the-instruction-sequence)
             (execute))
            ((eq message 'install-instruction-sequence)
             (lambda (seq) (setf the-instruction-sequence seq)))
            ((eq message 'allocate-register) #'allocate-register)
            ((eq message 'get-register) #'lookup-register)
            ((eq message 'install-operations)
             (lambda (ops) (setf the-ops (append the-ops ops))))
            ((eq message 'stack) stack)
            ((eq message 'operations) the-ops)
            (t (error "Unknown request -- MACHINE: ~S" message))))))))

(defun start (machine)
  (funcall machine 'start))

(defun get-register (machine register-name)
  (funcall (funcall machine 'get-register) register-name))

(defun get-register-contents (machine register-name)
  (get-contents (get-register machine register-name)))

(defun set-register-contents (machine register-name value)
  (set-contents (get-register machine register-name) value)
  'done)

(defun install-instruction (machine instructions)
  (funcall (funcall machine 'install-instruction-sequence) instructions))

(defun e_assemble (controller-text machine)
  (extract-labels controller-text
                  (lambda (insts labels)
                    (update-insts! insts labels machine))))

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

(defun update-insts! (insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (funcall machine 'stack))
        (ops (funcall machine 'operations)))
    (loop for inst in insts collect (set-instruction-execution-proc!
                                     inst
                                     (make-execution-procedure
                                      (instruction-text inst) labels machine
                                      pc flag stack ops)))))

(defun make-instruction (text)
  (cons text '()))

(defun instruction-text (inst)
  (car inst))

(defun set-instruction-execution-proc! (inst proc)
  (setf (cdr inst) proc)
  inst)

(defun make-label-entry (label-name insts labels)
  (when (assoc label-name labels)
    (error "Duplicate label definition: ~S" label-name))
  (cons label-name insts))

(defun lookup-label (all-labels label-name)
  (let ((val (assoc label-name all-labels)))
    (if val
        (cdr val)
        (error "Undefined label -- ASSEMBLE: ~S" label-name))))

(defun make-execution-procedure (inst labels machine
                                 pc flag stacks ops)
  (cond ((eq (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq (car inst) 'save)
         (make-save inst machine stacks pc))
        ((eq (car inst) 'restore)
         (make-restore inst machine stacks pc))
        ((eq (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        (t (error "Unknown instruction type -- ASSEMBLE : ~S" inst))))

(defun make-assign (inst machine labels operations pc)
  (let ((target (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (operation-exp value-exp)
               (make-operation-exp
                value-exp machine labels operations)
               (make-primitive-exp
                (car value-exp) machine labels))))
      (lambda ()
        (set-contents target (funcall value-proc))
        (advance-pc pc)))))

(defun assign-reg-name (assign-instruction)
  (cadr assign-instruction))

(defun assign-value-exp (assign-instruction)
  (cddr assign-instruction))

(defun advance-pc (pc)
  (set-contents pc (cdr (get-contents pc))))

(defun make-test (inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp condition)
        (let ((condition-proc
               (make-operation-exp
                condition machine labels operations)))
          (lambda ()
            (set-contents flag (funcall condition-proc))
            (advance-pc pc)))
        (error "Bad Test instruction -- ASSEMBLE: ~S" inst))))

(defun test-condition (test-instruction)
  (cdr test-instruction))

(defun make-branch (inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp dest)
        (let ((insts (lookup-label labels (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents pc insts)
                (advance-pc pc))))
        (error "Bad Branch instruction --ASSEMBLE: ~S" inst))))

(defun branch-dest (branch-instruction)
  (cadr branch-instruction))

(defun make-goto (inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp dest)
           (let ((insts (lookup-label labels
                                      (label-exp-label dest))))
             (lambda ()
               (set-contents pc insts))))
          ((register-exp dest)
           (let ((reg
                  (get-register machine (register-exp-reg dest))))
             (lambda () (set-contents pc (get-contents reg)))))
          (t (error "BAD GOTO instruction -- ASSEMBLE : ~S" inst)))))

(defun goto-dest (goto-instruction)
  (cadr goto-instruction))

(defun make-save (inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push-stack stack (get-contents reg))
      (advance-pc pc))))

(defun make-restore (inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents reg (pop-stack stack))
      (advance-pc pc))))

(defun stack-inst-reg-name (stack-instruction)
  (cadr stack-instruction))

(defun make-perform (inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp action)
        (let ((action-proc (make-operation-exp
                            action machine labels operations)))
          (lambda ()
            (funcall action-proc)
            (advance-pc pc))
          )
        (error "Bad PERFORM instruction -- ASSEMBLE: ~S" inst))))

(defun perform-action (inst) (cdr inst))

(defun make-primitive-exp (exp machine labels)
  (cond ((constant-exp exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp exp)
         (let ((insts (lookup-label labels
                                    (label-exp-label exp))))
           (lambda () insts)))
        ((register-exp exp)
         (let ((r (get-register machine (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (t (error "Unknown expression type -- ASSEMBLE: ~S" exp))))

(defun register-exp (exp) (tagged-list exp 'reg))

(defun tagged-list (exp prefix)
  (eq (car exp) prefix))

(defun register-exp-reg (exp) (cadr exp))

(defun constant-exp (exp) (tagged-list exp 'const))

(defun constant-exp-value (exp) (cadr exp))

(defun label-exp (exp) (tagged-list exp 'label))

(defun label-exp-label (exp) (cadr exp))

(defun make-operation-exp (exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs (mapcar (lambda (e) (make-primitive-exp e machine labels))
                        (operation-exp-operands exp))))
    (lambda () (apply op (mapcar (lambda (p) (funcall p)) aprocs)))))

(defun operation-exp (exp)
  (and (consp exp) (tagged-list (car exp) 'op)))

(defun operation-exp-op (operation-exp)
  (cadr (car operation-exp)))

(defun operation-exp-operands (operation-exp)
  (cdr operation-exp))

(defun lookup-prim (symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown operation -- ASSEMBLE: ~S" symbol))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; machine.lisp ends here
