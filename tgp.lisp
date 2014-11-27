;;; tgp.lisp ---
;;
;; Filename: tgp.lisp
;; Description:
;; Author: Liu Enze
;; Maintainer:
;; Created: Fri Nov 28 07:51:54 2014 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated: Fri Nov 28 07:51:54 2014 (+0800)
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


(in-package tgp)

(start (make-instance 'easy-acceptor :port 4001))

(defparameter *ajax-processor*
  (make-instance 'ajax-processor :server-uri "/ajax"))

(defun-ajax say-hi (name) (*ajax-processor*)
  (concatenate 'string "Hi " name ", nice to meet you."))

(define-easy-handler (say-yo :uri "/yo") ()
  (setf (content-type*) "text/plain")
  (format nil "Hey~A!" "brenda"))

(define-easy-handler (jsonp :uri "/jsonp") (func url)
  (setf (content-type*) "text/plain")
  (format nil "~A(~A)" func (flexi-streams:octets-to-string (drakma:http-request url))))

(setq *dispatch-table* (list 'dispatch-easy-handlers (create-ajax-dispatcher *ajax-processor*)))

(defmacro standard-page ((&key title script style) &body body)
  `(with-html-output-to-string
       (*standard-output* nil :prologue t :indent t)
     (:html :lang "en"
            (:head
             (:meta :charset "utf-8")
             (:title ,title)
             ,(when style
                    `(:style :type "text/css" (str ,style)) )
             ,(when script
                    `(:script :type "text/javascript" (str ,script))))
            (:body ,@body))))

(setf (html-mode) :html5)

(push (create-folder-dispatcher-and-handler "/style/" "~/code/lisp-practise/style/")
      *dispatch-table*)

(define-easy-handler (birth :uri "/birth") ()
  (standard-page (:title "Beautiful Brenda" :style "
div#div1{
    position:fixed;
    top:0;
    left:0;
    bottom:0;
    right:0;
    z-index:-1;
}
div#div1 > img {
    height:100%;
    width:100%;
    border:0;
}
")
    (:div :id "div1" (:img :src "/style/flower.jpg"))
    (:div :id "header"
          (:span "Wait for your important day coming"))
    (:h1 "Happy Birthday")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tgp.lisp ends here
