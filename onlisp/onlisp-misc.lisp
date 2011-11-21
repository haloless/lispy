

(defpackage #:onlisp.misc
  (:use #:cl #:onlisp))
(in-package :onlisp.misc)

;; chap 13
;; calculation at compile-time

(defmacro avg (&rest args)
  `(/ (+ ,@args) ,(length args)))
(defmacro most-of (&rest args)
  (let ((need (floor (/ (length args) 2)))
	(hits (gensym)))
    `(let ((,hits 0))
       (or ,@(mapcar #'(lambda (a)
			 `(and ,a (> (incf ,hits) ,need)))
		     args)))))

;; (defmacro nthmost (n lst)



    
(defconstant *segs* 20)
(defconstant *du* (/ 1.0 *segs*))
(defconstant *pts* (make-array (list (1+ *segs*) 2)))
(defmacro genbez (x0 y0 x1 y1 x2 y2 x3 y3)
  (with-gensyms (gx0 gx1 gy0 gy1 gx3 gy3)
		`(let ((,gx0 ,x0) (,gy0 ,y0)
		       (,gx1 ,x1) (,gy1 ,y1)
		       (,gx3 ,x3) (,gy3 ,y3))
		   (let ((cx (* (- ,gx1 ,gx0) 3))
			 (cy (* (- ,gy1 ,gy0) 3))
			 (px (* (- ,x2 ,gx1) 3))
			 (py (* (- ,y2 ,gy1) 3)))
		     (let ((bx (- px cx))
			   (by (- py cy))
			   (ax (- ,gx3 px ,gx0))
			   (ay (- ,gy3 py ,gy0)))
		       (setf (aref *pts* 0 0) ,gx0
			     (aref *pts* 0 1) ,gy0)
		       ,@(map1-n #'(lambda (n)
				     (let* ((u (* n *du*))
					    (u2 (* u u))
					    (u3 (expt u 3)))
				       `(setf (aref *pts* ,n 0)
					      (+ (* ax ,u3)
						 (* bx ,u2)
						 (* cx ,u)
						 ,gx0)
					      (aref *pts* ,n 1)
					      (+ (* ay ,u3)
						 (* by ,u2)
						 (* cy ,u)
						 ,gy0))))
				 (1- *segs*))
		       (setf (aref *pts* *segs* 0) ,gx3
			     (aref *pts* *segs* 1) ,gy3))))))



;; chap 21 multi-process
(defstruct proc pri state wait)
(proclaim '(special *procs* *proc*))
(defvar *halt* (gensym))
(defvar *default-proc*
  (make-proc :state #'(lambda (x)
			(format t "~%>> ")
			(princ (eval (read)))
			(pick-process))))
(defmacro fork (expr pri)
  `(prog1 ',expr
     (push (make-proc 
	    :state #'(lambda (,(gensym))
		       ,expr
		       (pick-process))
	    :pri ,pri)
	   *procs*)))
(defmacro program (name args &body body)
  `(=defun ,name ,args
	   (setq *procs* nil)
	   ,@body
	   (catch *halt* (loop (pick-process)))))
(defun pick-process ()
  (multiple-value-bind (p val) (most-urgent-process)
    (setq *proc* p
	  *procs* (delete p *procs*))
    (funcall (proc-state p) val)))
(defun most-urgent-process ()
  (let ((proc1 *default-proc*)
	(max -1)
	(val1 t))
    (dolist (p *procs*)
      (let ((pri (proc-pri p)))
	(if (> pri max)
	    (let ((val (or (not (proc-wait p))
			   (funcall (proc-wait p)))))
	      (when val
		(setq proc1 p
		      max pri
		      val1 val))))))
    (values proc1 val1)))
(defun arbitrator (test cont)
  (setf (proc-state *proc*) cont
	(proc-wait *proc*) test)
  (push *proc* *procs*)
  (pick-process))
(defmacro wait (parm test &body body)
  `(arbitrator #'(lambda () ,test)
	       #'(lambda (,parm) ,@body)))
(defmacro yield (&body body)
  `(arbitrator nil #'(lambda (,(gensym)) ,@body)))
(defun setpri (n) (setf (proc-pri *proc*) n))
(defun halt (&optional val)
  (throw *halt* val))
(defun kill (&optional obj &rest args)
  (if obj
      (setq *procs* (apply #'delete obj *procs* args))
      (pick-process)))

;; (defvar *open-doors* nil)
;; (=defun pedestrian ()
;; 	(wait d (car *open-doors*)
;; 	      (format t "Entering ~A~%" d)))
;; (program ped ()
;; 	 (fork (pedestrian) 1))


