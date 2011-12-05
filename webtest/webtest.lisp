;;;; webtest.lisp

(in-package #:webtest)

;;; "webtest" goes here. Hacks and glory await!

(defvar *server-acceptor* nil)

(defun start-server (&key port)
  (setf *server-acceptor*
	(make-instance 'hunchentoot:easy-acceptor :port port))
  (hunchentoot:start *server-acceptor*))

(defun stop-server ()
  (unwind-protect 
       (hunchentoot:stop *server-acceptor*)
    (setf *server-acceptor* nil)))

;; assumed to be load from webtest/ directory
(defun webtest-directory (directory)
  (let ((dir (translate-logical-pathname directory)))
    (make-pathname :device (pathname-device dir)
		   :host (pathname-host dir)
		   :directory (pathname-directory dir))))
(defparameter *webtest-this-file-true* *load-truename*)
(defparameter *webtest-directory*
  (webtest-directory *webtest-this-file-true*))

(defun webtest-pathname (pathname)
  (let ((dir-spec (parse-namestring pathname)))
    (make-pathname :directory (append (pathname-directory *webtest-directory*)
				      (cdr (pathname-directory dir-spec)))
		   :defaults *webtest-directory*)))
(defun set-webtest-logical-paths ()
  (setf (logical-pathname-translations "webtest")
	`(("**;*.*" ,(webtest-pathname "**/*.*")))))
				       
(defun init-webtest (&key (webtest-path-spec "."))
  "(init-webtest '/home/sxs/myworks/lisp/lispy/webtest/'"
  (setf *webtest-this-file-true*
	(merge-pathnames "webtest.lisp" webtest-path-spec))
  (setf *webtest-directory*
	(webtest-directory *webtest-this-file-true*))
  (set-webtest-logical-paths))
	
;; (defun webtest-pathname (logical-pathname)
;;   (translate-logical-pathname logical-pathname))
;; (defun webtest-pathnamestring (logical-pathname)
;;   (namestring (webtest-pathname logical-pathname)))

