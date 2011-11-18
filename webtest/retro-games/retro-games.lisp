

(in-package #:webtest)

;; (use-package :cl-who)
;; (use-package :hunchentoot)


;; data

(defvar *games* nil)

(defclass game ()
  ((name :reader name
	 :initarg :name)
   (votes :accessor votes
	  :initform 0
	  :type integer)
   ))

(defun make-game (game-name)
  (make-instance 'game :name game-name))

(defmethod vote-for (game)
  (incf (votes game)))

(defun game-from-name (game-name)
  (find game-name *games* :test #'string-equal :key #'name))

(defun game-stored? (game-name)
  (game-from-name game-name))

(defun add-game! (game-name)
  (unless (game-stored? game-name)
    (push (make-game game-name) *games*)))

(defun all-games ()
  (sort (copy-list *games*) #'> :key #'votes))

(defun setup-games ()
  (setf *games* nil)
  (add-game! "mario")
  (add-game! "contra")
  (add-game! "tetris"))


(elephant:defpclass persistent-game ()
  ((name :reader name
	 :initarg :name
	 :index t)
   (votes :accessor votes
	  :initarg :votes
	  :initform 0))
  )

(defparameter *persistent-class* 'persistent-game)
(defun make-game (game-name)
  (make-instance *persistent-class* :name game-name))
(defun game-from-name (game-name)
  (ele:get-instance-by-value *persistent-class* 'name game-name))
(defun add-game! (game-name)
  (ele:with-transaction ()
    (unless (game-stored? game-name)
      (make-game game-name))))
(defun all-games ()
  (sort (ele:get-instances-by-range *persistent-class* 'name nil nil)
	#'> :key #'votes))
	
;; (defclass pgame ()
;;   ((name :reader name
;; 	 :initarg :name
;; 	 :index t)
;;    (votes :accessor votes
;; 	  :initarg :votes
;; 	  :initform 0))
;;   (:metaclass elephant:persistent-metaclass))



;; web form

(defmacro std-page ((&key title (css nil) (logo nil)) &body body)
  (declare (ignorable css logo))
  `(cl-who:with-html-output-to-string
       (*standard-output* nil :prologue t :indent t)
     (:html :xmlns "http://www.w3.org/1999/xhtml"
	    :xml\:lang "en"
	    :lang "en"
	    (:head
	     (:meta :http-equiv "Content-Type"
		    :content "text/html;charset=utf-8")
	     (:title ,title)
	     ;; (:link :type "text/css"
	     ;; 	    :rel "stylesheet"
	     ;; 	    :href "/retro.css")
	     )
	    (:body
	     (:div :id "header"
		   (:img :src "/logo.jpg"
			 :alt "Commodore 64"
			 :class "logo")
		   (:span :class "strapline"
			  "Vote on ur favourite Retro Game"))
	     ,@body))))

(defmacro define-url-handler ((name &rest queries) &body body)
  (let ((ghandler (gensym (format nil "~A-handler" name)))
	(uri (format nil "/~A" name)))
    `(hunchentoot:define-easy-handler (,ghandler :uri ,uri)
	 ,queries
       ,@body)))
  

(defun make-retro-games-page ()
  (std-page (:title "Top Retro Games")
    (:h1 "Vote on Retro Games!!")
    (:p "Add a Missing Game " (:a :href "new-game" "here"))
    (:h2 "Current Stand")
    (:div :id "chart"
	  (:ol
	   (dolist (game (all-games))
	     (cl-who:htm
	      (:li
	       (:a :href (format nil "vote?name=~A" (name game)) "Vote!")
	       (cl-who:fmt
		"~A with ~D votes" (name game) (votes game)))))))))

;; main page
(defun setup-retro-games-page ()
  (hunchentoot:define-easy-handler (retro-games-handler :uri "/retro-games")
      ()
    (setf (hunchentoot:content-type*) "text/html")
    (make-retro-games-page)
    ))
;; vote page
(defun setup-vote-page ()
  (hunchentoot:define-easy-handler (vote-handler :uri "/vote")
      (name)
    (let ((game (game-from-name name)))
      (if game
	  (vote-for game))
    (hunchentoot:redirect "/retro-games"))))
;; add-a-game page
(defun setup-new-game-page ()
  (hunchentoot:define-easy-handler (add-new-game-handler :uri "/new-game")
      ()
    (std-page (:title "Add a New Game")
      (:h1 "Add a new game to the chart")
      (:form :action "/game-added" :method "post"
	     :onsubmit
	     (let ((js-check (parenscript:ps-inline*
			      '(when (= name.value "")
				(alert "Plz enter a name!")
				(return false)
				))))
	        js-check)
	       (:p "What is the name of the game?" (:br)
		   (:input :type "text" :name "name" :class "txt"))
	       (:p (:input :type "submit" :value "Add" :class "btn"))))))
;; game-add cgi page
(defun setup-game-added-page ()
  (hunchentoot:define-easy-handler
      (new-game-added-handler :uri "/game-added")
      (name)
    (unless (or (null name) (zerop (length name)))
      (add-game! name))
    (hunchentoot:redirect "/retro-games")))
  
    

(defun setup-all-pages-for-retro-games ()
  (setup-games)
  (setup-retro-games-page)
  (setup-vote-page)
  (setup-new-game-page)
  (setup-game-added-page))




;; persistant
(defvar *game-db* )
(defun open-game-db ()
  (setf *game-db*
	(elephant:open-store
	 `(:clsql
	   (:sqlite3
	    ,(namestring (translate-logical-pathname
			  "webtest:retro-games;games.db")))))))
;; for berkely DB
;; (open-store '(:BDB "/tmp/bdb/"))
(defun close-game-db ()
  (elephant:close-store *game-db*)
  (setf *game-db* nil))

