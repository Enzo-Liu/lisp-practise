;;; mente-carlo.lisp ---
;;
;; Filename: mente-carlo.lisp
;; Description:
;; Author: Liu Enze
;; Maintainer:
;; Created: Fri Nov 28 07:49:36 2014 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated: Fri Nov 28 07:49:36 2014 (+0800)
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


;;; lecture 5a in sicp
;;; talking about assign and state and environment evaluate model
;;; mente-carlo the calcuate pai

(defconstant +LIMIT+ 100000000)

(defun menta-carlo (trials fn)
  (/ (loop for i below trials
        counting (funcall fn) into passed
        finally (return passed)) trials))

(defun pai-test ()
  (= 1 (gcd (random +LIMIT+) (random +LIMIT+))))

(sqrt (/ 6 (menta-carlo 10000000 #'pai-test) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mente-carlo.lisp ends here
