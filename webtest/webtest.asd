;;;; webtest.asd

(asdf:defsystem #:webtest
  :serial t
  :depends-on (#:cl-who
               #:hunchentoot
               #:parenscript
               #:elephant
               #:cl-containers
	       #:clsql
	       #:clsql-sqlite3
	       #:ele-clsql
	       #:html-template
	       )
  
  :components ((:file "package")
               (:file "webtest")

	       (:module retro-games
			:components ((:file retro-games))
			:depends-on ("webtest"))
	       (:module blog
			:components ((:file blog))
			:depends-on ("webtest"))
	       ))

