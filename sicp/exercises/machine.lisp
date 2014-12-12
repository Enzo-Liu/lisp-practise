;;; machine.lisp ---
;;
;; Filename: machine.lisp
;; Description:
;; Author: Liu Enze
;; Maintainer:
;; Created: Thu Dec 11 14:39:16 2014 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated: Fri Dec 12 10:54:05 2014 (+0800)
;;           By: Liu Enze
;;     Update #: 4
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

;; (define (make-machine register-names ops controller-text)
;;     (let ((machine (make-new-machine)))
;;       (for-each (lambda (register-name)
;;                   ((machine 'allocate-register) register-name))
;;                 register-names)
;;       ((machine 'install-operations) ops)
;;       ((machine 'install-instruction-sequence)
;;        (assemble controller-text machine))
;;       machine))

(defmacro make-machine (registers ops controller-text)
  (let ((machine (make-new-machine)))
    (mapcar #'(lambda (register-name)
                (funcall
                 (funcall machine 'allocate-register) register-name))
            registers)
    (funcall (funcall machine 'install-operations) ops)
    (funcall (funcall machine 'install-instruction-sequence)
             (assemble controller-text machine))
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
           (list (list 'pc pc) (list 'flag flag))))
      (labels ((allocate-register (name)
                 (if (assoc name register-table)
                     (error "Multiply defined register: ~S" name)
                     (setf register-table
                           (cons (list name (make-register name))
                                 register-table)))
                 'register-allocated)
               (lookup-register (name)
                 (let ((val (assoc name register-table)))
                   (if val (cadr val) (error "Unknown register: ~S" name))))
               (execute ()
                 (let ((insts (get-contents pc)))
                   (if (null insts)
                       'done
                       (progn
                         (funcall (instruction-execution-proc (car insts)))
                         (execute))))))
        (lambda (message)
          (cond ((eq message 'start)
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

;; TODO have done till here
;; (define (assemble controller-text machine)
;;     (extract-labels controller-text
;;                     (lambda (insts labels)
;;                       (update-insts! insts labels machine)
;;                       insts)))

(defun assemble (controller-text machine)
  )

;; (define (extract-labels text receive)
;;     (if (null? text)
;;         (receive '() '())
;;         (extract-labels (cdr text)
;;                         (lambda (insts labels)
;;                           (let ((next-inst (car text)))
;;                             (if (symbol? next-inst)
;;                                 (receive insts
;;                                          (cons (make-label-entry next-inst
;;                                                                  insts)
;;                                                labels))
;;                                 (receive (cons (make-instruction next-inst)
;;                                                insts)
;;                                          labels)))))))

;; (define (update-insts! insts labels machine)
;;     (let ((pc (get-register machine 'pc))
;;           (flag (get-register machine 'flag))
;;           (stack (machine 'stack))
;;           (ops (machine 'operations)))
;;       (for-each
;;        (lambda (inst)
;;          (set-instruction-execution-proc!
;;           inst
;;           (make-execution-procedure
;;            (instruction-text inst) labels machine
;;            pc flag stack ops)))
;;        insts)))

;; (define (make-instruction text)
;;   (cons text '()))
;; (define (instruction-text inst)
;;   (car inst))
;; (define (instruction-execution-proc inst)
;;   (cdr inst))
;; (define (set-instruction-execution-proc! inst proc)
;;     (set-cdr! inst proc))

;; (define (make-label-entry label-name insts)
;;     (cons label-name insts))

;; (define (lookup-label labels label-name)
;;     (let ((val (assoc label-name labels)))
;;       (if val
;;           (cdr val)
;;           (error "Undefined label -- ASSEMBLE" label-name))))

;; (define (make-execution-procedure inst labels machine
;;                                   pc flag stack ops)
;;     (cond ((eq? (car inst) 'assign)
;;            (make-assign inst machine labels ops pc))
;;           ((eq? (car inst) 'test)
;;            (make-test inst machine labels ops flag pc))
;;           ((eq? (car inst) 'branch)
;;            (make-branch inst machine labels flag pc))
;;           ((eq? (car inst) 'goto)
;;            (make-goto inst machine labels pc))
;;           ((eq? (car inst) 'save)
;;            (make-save inst machine stack pc))
;;           ((eq? (car inst) 'restore)
;;            (make-restore inst machine stack pc))
;;           ((eq? (car inst) 'perform)
;;            (make-perform inst machine labels ops pc))
;;           (else (error "Unknown instruction type -- ASSEMBLE"
;;                        inst))))

;; (define (make-assign inst machine labels operations pc)
;;     (let ((target
;;            (get-register machine (assign-reg-name inst)))
;;           (value-exp (assign-value-exp inst)))
;;       (let ((value-proc
;;              (if (operation-exp? value-exp)
;;                  (make-operation-exp
;;                   value-exp machine labels operations)
;;                  (make-primitive-exp
;;                   (car value-exp) machine labels))))
;;         (lambda ()                ; execution procedure for assign
;;           (set-contents! target (value-proc))
;;           (advance-pc pc)))))

;; (define (assign-reg-name assign-instruction)
;;   (cadr assign-instruction))
;; (define (assign-value-exp assign-instruction)
;;     (cddr assign-instruction))

;; (define (advance-pc pc)
;;     (set-contents! pc (cdr (get-contents pc))))
;; (define (make-test inst machine labels operations flag pc)
;;   (let ((condition (test-condition inst)))
;;     (if (operation-exp? condition)
;;         (let ((condition-proc
;;                (make-operation-exp
;;                 condition machine labels operations)))
;;           (lambda ()
;;             (set-contents! flag (condition-proc))
;;             (advance-pc pc)))
;;         (error "Bad TEST instruction -- ASSEMBLE" inst))))
;; (define (test-condition test-instruction)
;;     (cdr test-instruction))
;; (define (make-branch inst machine labels flag pc)
;;   (let ((dest (branch-dest inst)))
;;     (if (label-exp? dest)
;;         (let ((insts
;;                (lookup-label labels (label-exp-label dest))))
;;           (lambda ()
;;             (if (get-contents flag)
;;                 (set-contents! pc insts)
;;                 (advance-pc pc))))
;;         (error "Bad BRANCH instruction -- ASSEMBLE" inst))))
;; (define (branch-dest branch-instruction)
;;     (cadr branch-instruction))
;; (define (make-goto inst machine labels pc)
;;   (let ((dest (goto-dest inst)))
;;     (cond ((label-exp? dest)
;;            (let ((insts
;;                   (lookup-label labels
;;                                 (label-exp-label dest))))
;;              (lambda () (set-contents! pc insts))))
;;           ((register-exp? dest)
;;            (let ((reg
;;                   (get-register machine
;;                                 (register-exp-reg dest))))
;;              (lambda ()
;;                (set-contents! pc (get-contents reg)))))
;;           (else (error "Bad GOTO instruction -- ASSEMBLE"
;;                        inst)))))
;; (define (goto-dest goto-instruction)
;;   (cadr goto-instruction))


;; (define (make-save inst machine stack pc)
;;     (let ((reg (get-register machine
;;                              (stack-inst-reg-name inst))))
;;       (lambda ()
;;         (push stack (get-contents reg))
;;         (advance-pc pc))))
;; (define (make-restore inst machine stack pc)
;;   (let ((reg (get-register machine
;;                            (stack-inst-reg-name inst))))
;;     (lambda ()
;;       (set-contents! reg (pop stack))
;;       (advance-pc pc))))
;; (define (stack-inst-reg-name stack-instruction)
;;   (cadr stack-instruction))

;; (define (make-perform inst machine labels operations pc)
;;     (let ((action (perform-action inst)))
;;       (if (operation-exp? action)
;;           (let ((action-proc
;;                  (make-operation-exp
;;                   action machine labels operations)))
;;             (lambda ()
;;               (action-proc)
;;               (advance-pc pc)))
;;           (error "Bad PERFORM instruction -- ASSEMBLE" inst))))
;; (define (perform-action inst) (cdr inst))


;; (define (make-primitive-exp exp machine labels)
;;     (cond ((constant-exp? exp)
;;            (let ((c (constant-exp-value exp)))
;;              (lambda () c)))
;;           ((label-exp? exp)
;;            (let ((insts
;;                   (lookup-label labels
;;                                 (label-exp-label exp))))
;;              (lambda () insts)))
;;           ((register-exp? exp)
;;            (let ((r (get-register machine
;;                                   (register-exp-reg exp))))
;;              (lambda () (get-contents r))))
;;           (else
;;            (error "Unknown expression type -- ASSEMBLE" exp))))

;; (define (register-exp? exp) (tagged-list? exp 'reg))
;; (define (register-exp-reg exp) (cadr exp))
;; (define (constant-exp? exp) (tagged-list? exp 'const))
;; (define (constant-exp-value exp) (cadr exp))
;; (define (label-exp? exp) (tagged-list? exp 'label))
;; (define (label-exp-label exp) (cadr exp))
;; (define (make-operation-exp exp machine labels operations)
;;     (let ((op (lookup-prim (operation-exp-op exp) operations))
;;           (aprocs
;;            (map (lambda (e)
;;                   (make-primitive-exp e machine labels))
;;                 (operation-exp-operands exp))))
;;       (lambda ()
;;         (apply op (map (lambda (p) (p)) aprocs)))))
;; (define (operation-exp? exp)
;;   (and (pair? exp) (tagged-list? (car exp) 'op)))
;; (define (operation-exp-op operation-exp)
;;   (cadr (car operation-exp)))
;; (define (operation-exp-operands operation-exp)
;;     (cdr operation-exp))
;; (define (lookup-prim symbol operations)
;;     (let ((val (assoc symbol operations)))
;;       (if val
;;           (cadr val)
;;           (error "Unknown operation -- ASSEMBLE" symbol))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; machine.lisp ends here
