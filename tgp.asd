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
