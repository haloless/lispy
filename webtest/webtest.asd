;;;; webtest.asd

(asdf:defsystem #:webtest
  :serial t
  :depends-on (#:cl-who
               #:hunchentoot
               #:parenscript
               #:elephant
               #:cl-containers)
  :components ((:file "package")
               (:file "webtest")))

