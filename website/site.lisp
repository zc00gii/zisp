(defmacro html-to-string (&body body) `(with-html-output-to-string
       (*standard-output* nil :indent t) ,@body))

(defmacro html-to-string-no-indent (&body body) `(with-html-output-to-string
       (*standard-output*) ,@body))

(defun start-site (site-content)
  (hunchentoot:start (make-instance 'hunchentoot:acceptor :port 8080))
   (define-easy-handler (home :uri "/") (text)
     (setf (hunchentoot:content-type*) "text/html")
     site-content))

(setf site-main (html-to-string
  (:html
   (:head
    (:title "zc00gii's test site"))
   (:body
    (:h1 "Welcome to my hunchentoot test site")
    (:h2 "Links")
    (:p
     (:a :href "The Facepalm" "http://www.thefacepalm.net/")
     (:br)
     (:a :href "Liquid" "http://www.liquid-de.org/"))
    (:br)
    (:h2 "Other stuff")
    (:p
     (format *standard-output* "Random number: ~a" (random 10000))
     (:br)
     (format *standard-output* "Text entered: ~a" text))))))
