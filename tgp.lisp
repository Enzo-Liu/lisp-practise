(in-package tgp)

(start (make-instance 'easy-acceptor :port 4242))

(defparameter *ajax-processor*
    (make-instance 'ajax-processor :server-uri "/ajax"))

(defun-ajax say-hi (name) (*ajax-processor*)
    (concatenate 'string "Hi " name ", nice to meet you."))

(define-easy-handler (say-yo :uri "/yo") ()
    (setf (content-type*) "text/plain")
      (format nil "Hey~A!" "sdfsdfsdfsdfsdf"))

(define-easy-handler (jsonp :uri "/jsonp") (func url)
    (setf (content-type*) "text/plain")
      (format nil "~A(~A)" func (flexi-streams:octets-to-string (drakma:http-request url))))

(setq *dispatch-table* (list 'dispatch-easy-handlers (create-ajax-dispatcher *ajax-processor*)))

(defmacro standard-page ((&key title script) &body body)
  "All pages on the Retro Games site will use the following macro;
   less to type and a uniform look of the pages (defines the header
   and the stylesheet).
   The macro also accepts an optional script argument. When present, the
   script form is expected to expand into valid JavaScript."
  `(with-html-output-to-string
    (*standard-output* nil :prologue t :indent t)
    (:html :lang "en"
           (:head
            (:meta :charset "utf-8")
            (:title ,title)
            (:link :type "text/css"
                   :rel "stylesheet"
                   :href "/retro.css")
            ,(when script
               `(:script :type "text/javascript"
                         (str ,script))))
           (:body
            (:div :id "header" ; Retro games header
                  (:img :src "/logo.jpg"
                        :alt "Commodore 64"
                        :class "logo")
                  (:span :class "strapline"
                         "Vote on your favourite Retro Game"))
            ,@body))))

(setf (html-mode) :html5)

(define-easy-handler (new-game :uri "/new-game") ()
  (standard-page (:title "Add a new game"
                         :script (ps  ; client side validation
                                  (defvar add-form nil)
                                  (defun validate-game-name (evt)
                                    "For a more robust event handling
                                     mechanism you may want to consider
                                     a library (e.g. jQuery) that encapsulates
                                     all browser-specific quirks."
                                    (when (= (@ add-form name value) "")
                                      (chain evt (prevent-default))
                                      (alert "Please enter a name.")))
                                  (defun init ()
                                    (setf add-form (chain document
                                                          (get-element-by-id "addform")))
                                    (chain add-form
                                           (add-event-listener "submit" validate-game-name false)))
                                  (setf (chain window onload) init)))
     (:h1 "Add a new game to the chart")
     (:form :action "/game-added" :method "post" :id "addform"
            (:p "What is the name of the game?" (:br)
                (:input :type "text" :name "name" :class "txt"))
            (:p (:input :type "submit" :value "Add" :class "btn")))))



