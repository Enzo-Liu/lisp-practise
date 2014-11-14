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
