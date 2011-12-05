;;;; phpblog.lisp

(in-package #:phpblog)

;;; "phpblog" goes here. Hacks and glory await!


(defvar *phpblog-root-path*)

(defvar *phpblog-log-stream*)


;; this file is used for initialization


(defun init-phpblog-logical-pathname 
    ;; (&key (root-dir-spec "."))
    ()
  "e.g. (init-phpblog-logical-pathname '/home/sxs/myworks/lisp/lispy/phpblog/')"
  ;; (setf *phpblog-root-path* (truename root-dir-spec))
  (setf (logical-pathname-translations "phpblog")
	`(
	  ("db;*" ,(merge-pathnames "data/db/*.s3db" *phpblog-root-path*))
	  ;; this pattern should be always be the last
	  ("**;*.*" ,(merge-pathnames "**/*.*" *phpblog-root-path*))))
  )

(defun init-phpblog-clhttpd ()
  (let ((clhttpd-root-pathname
	 (make-pathname :directory
			(append (butlast (pathname-directory *phpblog-root-path*))
				'("cl-http-binghe"))))
	)
    (load (merge-pathnames "contrib/kpoeck/port-template/load.lisp" 
			   clhttpd-root-pathname))
    (funcall 'cl-user::compile-all)
    ))


(eval-when (:execute :load-toplevel)
  (let ((this-file (load-time-value 
		    (or #.*compile-file-pathname* *load-pathname*)))
	)
    (format t "~A~%" this-file)
    (setf *phpblog-root-path*
	  (make-pathname :directory (pathname-directory this-file)))
    (format t "~A:~A~%" (type-of *phpblog-root-path*) *phpblog-root-path*)

    (init-phpblog-logical-pathname) 
    (init-phpblog-clhttpd)
    ))


;; (defun init-phpblog ()
  


  

(defun clsql-connect (dbname)
  (let ((logical-dbname (concatenate 'string "phpblog:db;" dbname))
	)
  (clsql:connect (list (namestring (translate-logical-pathname logical-dbname)))
		 :database-type :sqlite3)
  ))
