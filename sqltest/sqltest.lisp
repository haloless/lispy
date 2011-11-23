;;;; sqltest.lisp

(in-package #:sqltest)

;;; "sqltest" goes here. Hacks and glory await!


(clsql:def-view-class employee ()
  ((emplid :db-kind :key
	   :db-constraints :not-null
	   :type integer
	   :initarg :emplid)
   (first-name :accessor first-name
	       :type (string 32)
	       :initarg :first-name)
   (last-name :accessor last-name
	      :type (string 32)
	      :initarg :last-name)
   (email :accessor employee-email
	  :type (string 128)
	  :nulls-ok t
	  :initarg :email)
   (companyid :type integer
	      :initarg :companyid)
   (company :accessor employee-company
	    :db-kind :join
	    :db-info (:join-class 
		      company
		      :home-key companyid
		      :foreign-key companyid
		      :set nil))
   (managerid :type integer
	      :nulls-ok t
	      :initarg :managerid)
   (manager :accessor employee-manager
	    :db-kind :join
	    :db-info (:join-class 
		      employee
		      :home-key managerid
		      :foreign-key emplid
		      :set nil))
   )
  (:base-table employee))
	       
(clsql:def-view-class company ()
  ((companyid :db-kind :key
	      :db-constraints :not-null
	      :type integer
	      :initarg :companyid)
   (name :type (string 128)
	 :initarg :name)
   (presidentid :type integer
		:initarg :presidentid)
   (employees :reader company-employees
	      :db-kind :join
	      :db-info (:join-class 
			employee 
			:home-key companyid
			:foreign-key companyid
			:set t))
   (president :reader president
	      :db-kind :join
	      :db-info (:join-class
			employee
			:home-key presidentid
			:foreign-key emplid
			:set nil))
   )
  (:base-table company))
    
(defun connect-db-for-test ()
  (clsql:connect '("/home/sxs/myworks/lisp/lispy/sqltest/model.s3db")
		 :database-type :sqlite3)
  )

(defun create-db-data-models ()
  "Ensure a database connection before this."
  (clsql:create-view-from-class 'employee)
  (clsql:create-view-from-class 'company))

(defvar *company1*)
(defvar *employee1*)
(defvar *employee2*)

(defun create-data-for-test ()
  (setf *company1* (make-instance 'company 
				  :companyid 1
				  :presidentid 1
				  :name "Widgets Inc.")
	*employee1* (make-instance 'employee
				   :emplid 1
				   :first-name "Vladimir"
				   :last-name "Lenin"
				   :email "lenin@soviet.org"
				   :companyid 1)
	*employee2* (make-instance 'employee
				   :emplid 2
				   :first-name "Josef"
				   :last-name "Stalin"
				   :email "stalin@soviet.org"
				   :companyid 1
				   :managerid 1)
	))
(defun insert-data-for-test ()
  (clsql:update-records-from-instance *employee1*)
  (clsql:update-records-from-instance *employee2*)
  (clsql:update-records-from-instance *company1*)
  )

				  
				  


