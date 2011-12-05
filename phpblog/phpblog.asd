;;;; phpblog.asd

(asdf:defsystem #:phpblog
  :serial t
  :depends-on (
	       ;; #:hunchentoot
               #:html-template
               #:clsql
               #:clsql-sqlite3
               #:vecto)
  :components ((:file "package")
               (:file "phpblog")))

