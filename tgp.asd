;;; tgp.asd ---
;;
;; Filename: tgp.asd
;; Description:
;; Author: Liu Enze
;; Maintainer:
;; Created: Fri Nov 28 07:51:36 2014 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated: Fri Nov 28 07:51:36 2014 (+0800)
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


;;;;
;;;; tgp.asd
;;;;
;;;; (asdf:load-system :tgp)
;;;; Enze.Liu
;;;; 29.06.2014
;;;;

(in-package :asdf)

(defsystem :tgp
  :name "tgp"
  :author "Enze.Liu (liuenze6516@gmail.com)"
  :version "0.1.0"
  :licence "MIT"
  :description ""
  :components ((:file "package")
               (:file "tgp" :depends-on ("package")))
  :depends-on (:drakma :cl-who :parenscript :ht-simple-ajax :hunchentoot ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tgp.asd ends here
