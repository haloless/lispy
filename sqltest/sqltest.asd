;;;; sqltest.asd

(asdf:defsystem #:sqltest
  :serial t
  :depends-on (#:clsql
               #:clsql-sqlite3
               #:hunchentoot
               #:html-template)
  :components ((:file "package")
               (:file "sqltest")))

