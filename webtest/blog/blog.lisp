

(in-package #:webtest)

(defvar *elephant-store*)
(defvar *blog-posts*)


(defun retrieve-blog-posts ()
  (ele:get-from-root "blog-posts" :sc *elephant-store*))
(defun make-blog-posts ()
  (ele:make-pset :sc *elephant-store*))
(defun ensure-blog-posts ()
  (or (retrieve-blog-posts)
      (let ((blog-posts (make-blog-posts)))
	(ele:add-to-root "blog-posts" blog-posts :sc *elephant-store*)
	blog-posts)))

(defun get-blog-posts-list (blog-posts)
  (ele:pset-list blog-posts))

(defun init-blog-db-system ()
  "run this after webtest system is initialized"
  (setf *elephant-store*
	(ele:open-store
	 `(:clsql
	   (:sqlite3
	    ,(namestring (translate-logical-pathname 
			  "webtest:blog;blog.db"))))))
  ;; (setf *blog-posts*
  ;; 	(or (ele:get-from-root "blog-posts")
  ;; 	    (let ((blog-posts (ele:make-pset)))
  ;; 	      (ele:add-to-root "blog-posts" blog-posts)
  ;; 	      blog-posts)))
  )
(defun close-blog-db-system ()
  (unwind-protect
       (progn (ele:close-store *elephant-store*))
    (setf *elephant-store* nil
	  *blog-posts* nil)))
		    
       


(ele:defpclass blog-post ()
  ((title :initarg :title
	  :accessor title)
   (body :initarg :body
	 :accessor body)
   (timestamp :initarg :timestamp
	      :accessor timestamp
	      :initform (get-universal-time)
	      :index t)
   (url-part :initarg :url-part
	     :accessor url-part
	     :initform nil
	     :index t)
   ))
   
(defmethod initialize-instance :after ((obj blog-post) &key)
  (when (eq nil (url-part obj))
    (setf (url-part obj)
	  (make-url-part (title obj)))))

(defun make-url-part (title)
  "generate url-part from a title."
  (string-downcase
   (delete-if #'(lambda (c) (not (or (alphanumericp c) (char= #\- c))))
	      (substitute #\- #\Space title))))



;; (defun generate-index-page ()
;;   (with-output-to-string (stream)
;;     (html-template:fill-and-print-template
;;      (translate-logical-pathname "webtest:blog;blog-template.html")
;;      `(:blog-posts
;;        ,(loop for post in (get-blog-posts-list *blog-posts*)
;; 	     collect `(:title ,(title post) :body ,(body post))))
;;      :stream stream)))
(defun generate-index-page ()
  (with-output-to-string (stream)
    (html-template:fill-and-print-template
     (translate-logical-pathname "webtest:blog;blog-index.html")
     `(:blog-posts
       ,(loop for post
	   in (nreverse (ele:get-instances-by-range 'blog-post 'timestamp nil nil))
	   collect (list :title (title post)
			 :body (body post)
			 :url-part (url-part post))))
     :stream stream)))
	     
(defun generate-blog-post-page ()
  (let ((url-part (hunchentoot:query-string hunchentoot:*request*)))
    (with-output-to-string (stream)
      (let ((blog-post (ele:get-instance-by-value 'blog-post 'url-part url-part)))
	(html-template:fill-and-print-template
	 (translate-logical-pathname "webtest:blog;blog-post.html")
	 `(:title ,(title blog-post) :body ,(body blog-post))
	 :stream stream)))))
  
	    
     
(defun export-blog-pages ()
  ;; (push (hunchentoot:create-prefix-dispatcher "/blog" 'generate-index-page)
  ;; 	hunchentoot:*dispatch-table*)
  (let ((blog-dispatch-table
	 (list (hunchentoot:create-regex-dispatcher "^/blog$" 'generate-index-page)
	       (hunchentoot:create-regex-dispatcher "^/blog/view/" 'generate-blog-post-page))))
    (setf hunchentoot:*dispatch-table*
	  (append blog-dispatch-table hunchentoot:*dispatch-table*))))





