;;;; onlisp.lisp

(in-package #:onlisp)

;;; "onlisp" goes here. Hacks and glory await!





;; chap 04
(proclaim '(inline last1 single append1 conc1 mklist))


(defun last1 (lst) (car (last lst)))
(defun single (lst)
  (and (consp lst) (not (cdr lst))))
(defun append1 (lst obj)
  (append lst (list obj)))
(defun conc1 (lst obj)
  (nconc lst (list obj)))
(defun mklist (obj)
  (if (listp obj) obj (list obj)))

(defun longer (x y)
  (labels ((compare (x y)
		    (and (consp x)
			 (or (null y)
			     (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
	(compare x y)
        (> (length x) (length y)))))
(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
	(if val (push val acc))))
    (nreverse acc)))
(defun group (source n)
  (if (zerop n) (error "zero length -- GROUP"))
  (labels ((rec (source acc)
		(let ((rest (nthcdr n source)))
		  (if (consp rest)
		      (rec rest (cons (subseq source 0 n) acc))
		      (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

(defun flatten (x)
  (labels ((rec (x acc)
		(cond ((null x) acc)
		      ((atom x) (cons x acc))
		      (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))
(defun prune (test tree)
  (labels ((rec (tree acc)
		(cond ((null tree) (nreverse acc))
		      ((consp (car tree))
		       (rec (cdr tree)
			    (cons (rec (car tree) nil) acc)))
		      (t (rec (cdr tree)
			      (if (funcall test (car tree))
				  acc
				  (cons (car tree) acc)))))))
    (rec tree nil)))
(defun prune-2 (test tree)
  (labels ((rec (tree)
		(cond ((null tree) nil)
		      ((consp (car tree))
		       (cons (rec (car tree))
			     (rec (cdr tree))))
		      (t (if (funcall test (car tree))
			     (rec (cdr tree))
			     (cons (car tree) 
				   (rec (cdr tree))))))))
    (rec tree)))
				  
(defun find2 (fn lst)
  (if (null lst) 
      nil
      (let ((val (funcall fn (car lst))))
	(if val
	    (values (car lst) val)
	    (find2 fn (cdr lst))))))
(defun before (x y lst &key (test #'eql))
  (and lst
       (let ((first (car lst)))
	 (cond ((funcall test y first) nil)
	       ((funcall test x first) lst)
	       (t (before x y (cdr lst) :test test))))))
(defun after (x y lst &key (test #'eql))
  (let ((rest (before y x lst :test test)))
    (and rest (member x rest :test test))))
(defun duplicate (obj lst &key (test #'eql))
  (member obj (cdr (member obj lst :test test)) :test test))
(defun split-if (fn lst)
  (let ((acc nil))
    (do ((src lst (cdr src)))
	((or (null src) (funcall fn (car src)))
	 (values (nreverse acc) src))
      (push (car src) acc))))

(defun most (fn lst)
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
	     (max (funcall fn wins)))
	(dolist (obj (cdr lst))
	  (let ((score (funcall fn obj)))
	    (when (> score max)
	      (setq wins obj
		    max score))))
	(values wins max))))
(defun best (fn lst)
  (if (null lst)
      nil
      (let ((wins (car lst)))
	(dolist (obj (cdr lst))
	  (if (funcall fn obj wins)
	      (setq wins obj)))
	wins)))
(defun mostn (fn lst)
  (if (null lst)
      (values nil nil)
      (let ((result (list (car lst)))
	    (max (funcall fn (car lst))))
	(dolist (obj (cdr lst))
	  (let ((score (funcall fn obj)))
	    (cond ((> score max)
		   (setq max score
			 result (list obj)))
		  ((= score max)
		   (push obj result)))))
	(values (nreverse result) max))))

(defun map-> (fn start test-fn succ-fn)
  (do ((i start (funcall succ-fn i))
       (result nil))
      ((funcall test-fn i) (nreverse result))
    (push (funcall fn i) result)))
(defun mappend (fn &rest lsts)
  (apply #'append (apply #'mapcar fn lsts)))
(defun mapcars (fn &rest lsts)
  (let ((result nil))
    (dolist (lst lsts)
      (dolist (obj lst)
	(push (funcall fn obj) result)))
    (nreverse result)))
(defun rmapcar (fn &rest args)		;recursive-mapcar
  (if (some #'atom args)
      (apply fn args)
      (apply #'mapcar
	     #'(lambda (&rest args)
		 (apply #'rmapcar fn args))
	     args)))
	      
(defun readlist (&rest args)
  (values (read-from-string
	   (concatenate 'string "("
			(apply #'read-line args)
			")"))))
(defun prompt (&rest args)
  (apply #'format *query-io* args)
  (read *query-io*))
(defun break-loop (fn quit &rest args)
  (format *query-io* "Entering break-loop.'~%")
  (loop 
   (let ((in (apply #'prompt args)))
     (if (funcall quit in)
	 (return)
         (format *query-io* "~A~%" (funcall fn in))))))

(defun mkstr (&rest args)
  (with-output-to-string (s)
			 (dolist (a args)
			   (princ a s))))
(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))
(defun reread (&rest args)
  (values (read-from-string (apply #'mkstr args))))
(defun explode (sym)
  (map 'list #'(lambda (c)
		 (intern (make-string 1
				      :initial-element c)))
       (symbol-name sym)))


       








;; chap 05

(defvar *!equivs* (make-hash-table))

(defun ! (fn)
  (or (gethash fn *!equivs*) fn))

(defun def! (fn fn!)
  (setf (gethash fn *!equivs*) fn!))


(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
	(multiple-value-bind (val win) (gethash args cache)
	  (if win
	      val
	      (setf (gethash args cache)
		    (apply fn args)))))))

(defun compose (&rest fns)
  (if fns
      (let ((fn1 (car (last fns)))
	    (fns (butlast fns)))
	#'(lambda (&rest args)
	    (reduce #'funcall fns
		    :from-end t
		    :initial-value (apply fn1 args))))
      #'identity))

(defun fif (if then &optional else)
  #'(lambda (x)
      (if (funcall if x)
	  (funcall then x)
	  (if else 
	      (funcall else x)))))
(defun fint (fn &rest fns)		;function intersection
  (if (null fns)
      fn
      (let ((chain (apply #'fint fns)))
	#'(lambda (x)
	    (and (funcall fn x)
		 (funcall chain x))))))
(defun fun (fn &rest fns)		;function union
  (if (null fns)
      fn
      (let ((chain (apply #'fun fns)))
	#'(lambda (x)
	    (or (funcall fn x) 
		(funcall chain x))))))

(defun lrec (rec &optional base)	;list recurser
  (labels ((self (lst)
		 (if (null lst)
		     (if (functionp base)
			 (funcall base)
		         base)
		     (funcall rec (car lst)
			      #'(lambda ()
				  (self (cdr lst)))))))
    #'self))

(setq our-length (lrec #'(lambda (x cont)
			   (1+ (funcall cont))) 0)
      out-every-is-oddp (lrec #'(lambda (x cont)
				  (and (oddp x)
				       (funcall cont)))
			      t))

(defun ttrav (rec &optional (base #'identity)) ;tree traverser
  (labels ((self (tree)
		 (if (atom tree)
		     (if (functionp base)
			 (funcall base tree)
		         base)
		     (funcall rec (self (car tree))
			      (if (cdr tree)
				  (self (cdr tree)))))))
    #'self))
(setq our-copy-tree (ttrav #'cons)
      count-leaves (ttrav #'(lambda (l r) (+ l (or r 1))) 1)
      flatten (ttrav #'nconc #'mklist))

(defun trec (rec &optional (base #'identity))
  (labels
      ((self (tree)
	     (if (atom tree)
		 (if (functionp base)
		     (funcall base tree)
		     base)
	         (funcall rec tree
			  #'(lambda () (self (car tree))) ;left
			  #'(lambda ()			  ;right
			      (if (cdr tree)
				  (self (cdr tree))))))))
    #'self))
(setq flatten-2 (trec #'(lambda (root left right)
			  (nconc (funcall left) (funcall right))))
      rfind-if-odd (trec #'(lambda (root left right)
			     (or (funcall left) (funcall right)))
			 #'(lambda (tree) (and (oddp tree) tree))))

	     





;; chap 06 functions as expressions

;; (defun *nodes* (make-hash-table))
;; (defun defnode (name contents &optional yes no)
;;   (setf (gethash name *nodes*)
;; 	(if yes
;; 	    #'(lambda ()
;; 		(format t "~A~%>> " contents)
;; 		(case (read)
;; 		  (yes (funcall (gethash yes *nodes*)))
;; 		  (t   (funcall (gethash no  *nodes*)))))
;; 	    #'(lambda () contents))))
(defvar *nodes* nil)
(defun defnode (&rest args)		;args: name contents yes no
  (push args *nodes*)
  args)
(defun compile-net (root)
  (let ((node (assoc root *nodes*)))
    (if (null node)
	nil
        (let ((contents (second node))
	      (yes (third node))
	      (no (fourth node)))
	  (if yes
	      (let ((yes-fn (compile-net yes))
		    (no-fn (compile-net no)))
		#'(lambda ()
		    (format t "~A~%>> " contents)
		    (funcall (if (eq (read) 'yes)
				 yes-fn
			         no-fn))))
	      #'(lambda () contents))))))




;; chap07 macro

(defmacro nif (expr pos zero neg)
  `(case (truncate (signum ,expr))
     (1 ,pos)
     (0 ,zero)
     (-1 ,neg)))
(defmacro mac (expr)
  `(pprint (macroexpand-1 ',expr)))

(defmacro our-expander (name)
  `(get ,name 'expander))
(defmacro our-defmacro (name parms &body body)
  (let ((g (gensym)))
    `(progn
       (setf (our-expander ',name)
	     #'(lambda (,g)
		 (block ,name
		   (destructuring-bind ,parms (cdr ,g)
		     ,@body))))
       ',name)))
(defun our-macroexpand-1 (expr)
  (if (and (consp expr) (our-expander (car expr)))
      (funcall (our-expander (car expr)) expr)
      expr))


(defmacro our-do (bindforms (test &rest result) &body body)
  (let ((label (gensym)))
    `(prog ,(make-initforms bindforms)
	   ,label
	   (if ,test
	       (return (progn ,@result)))
	   ,@body
	   (psetq ,@(make-stepforms bindforms))
	   (go ,label))))
(defun make-initforms (bindforms)
  (mapcar #'(lambda (b)
	      (if (consp b)
		  (list (car b) (cadr b))
		  (list b nil)))
	  bindforms))
(defun make-stepforms (bindforms)
  (mapcan #'(lambda (b)
	      (if (and (consp b) (third b))
		  (list (car b) (third b))
		  nil))
	  bindforms))

(defmacro with-redraw ((var objs) &body body)
  (let ((gob (gensym))
	(x0 (gensym)) (y0 (gensym))
	(x1 (gensym)) (y1 (gensym)))
    `(let ((,gob ,objs))
       (multiple-value-bind (,x0 ,y0 ,x1 ,y1) (bounds ,gob)
	 (dolist (,var ,gob) ,@body)
	 (multiple-value-bind (xa ya xb yb) (boudns ,gob)
	   (redraw (min ,x0 xa) (min ,y0 ya)
		   (max ,x1 xb) (max ,y1 yb)))))))
(defun move-objs (objs dx dy)
  (with-redraw (o objs)
	       (incf (obj-x o) dx)
	       (incf (obj-y o) dy)))
(defun scale-objs (objs factor)
  (with-redraw (o objs)
	       (setf (obj-dx o) (* (obj-dx o) factor)
		     (obj-dy o) (* (obj-dy o) factor))))

(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
	  (,gstop ,stop))
	 ((> ,var ,gstop))
       ,@body)))

(defmacro nthe (n lst)
  `(labels ((nth-fn (n lst)
		    (if (= n 0)
			(car lst)
		        (nth-fn (- n 1) (cdr lst)))))
     (nth-fn ,n ,lst)))

(defmacro orb (&rest args)
  (if (null args)
      nil
      (let ((sym (gensym)))
	`(let ((,sym ,(car args)))
	   (if ,sym
	       ,sym
	       (orb ,@(cdr args)))))))




(defmacro our-let (binds &body body)
  `((lambda ,(mapcar #'(lambda (x)
			 (if (consp x) (car x) x))
		     binds)
      ,@body)
    ,@(mapcar #'(lambda (x)
		  (if (consp x) (cadr x) nil))
	      binds)))

(defmacro when-bind ((var expr) &body body)
  `(let ((,var ,expr))
     (when ,var
       ,@body)))
(defmacro when-bind* (binds &body body)
  (if (null binds)
      `(progn ,@body)
      `(let (,(car binds))
	 (if ,(cadr binds)
	     (when-bind* ,(cdr binds) ,@body)))))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
		     `(,s (gensym)))
		 syms)
     ,@body))

(defmacro condlet (clauses &body body)
  (let ((bodfn (gensym))
	(vars (mapcar #'(lambda (v) (cons v (gensym)))
		      (remove-duplicates
		       (mapcar #'car 
			       (mappend #'cdr clauses))))))
    `(labels ((,bodfn ,(mapcar #'car vars)
		      ,@body))
       (cond ,@(mapcar #'(lambda (cl)
			   (condlet-clause vars cl bodfn))
		       clauses)))))
(defun condlet-clause (vars clause bodfn)
  `(,(car clause) (let ,(mapcar #'cdr vars)
		    (let ,(condlet-binds vars cl)
		      (,bodfn ,@(mapcar #'cdr vars))))))
(defun condlet-binds (vars cl)
  (mapcar #'(lambda (bindform)
	      (if (consp bindform)
		  (cons (cdr (assoc (car bindform) vars))
			(cdr bindform))))
	  (cdr cl)))

(defmacro with-db (db &body body)
  (let ((temp (gensym)))
    `(let ((,temp *db*))
       (unwind-protect
	   (progn
	     (setq *db* ,db)
	     (lock *db*)
	     ,@body)
	 (progn
	   (release *db*)
	   (setq *db* ,temp))))))

(defmacro if3 (test t-case nil-case ?-case)
  `(case ,test
     ((nil) ,nil-case)
     (?     ,?-case)
     (t     ,t-case)))
(defmacro nif (expr pos zero neg)	;numeric if
  (let ((g (gensym)))
    `(let ((,g ,expr))
       (cond ((plusp ,g) ,pos)
	     ((zerop ,g) ,zero)
	     (t ,neg)))))

(defmacro in (obj &rest choices)
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       (or ,@(mapcar #'(lambda (c) `(eql ,insym ,c))
		     choices)))))
(defmacro inq (obj &rest args)
  `(in ,obj ,@(mapcar #'(lambda (a)
			  `',a)
		      args)))
(defmacro in-if (fn &rest choices)
  (let ((fnsym (gensym)))
    `(let ((,fnsym ,fn))
       (or ,@(mapcar #'(lambda (c)
			 `(funcall ,fnsym ,c))
		     choices)))))
(defmacro >case (expr &rest clauses)
  (let ((g (gensym)))
    `(let ((,g ,expr))
       (cond ,@(mapcar #'(lambda (cl) (>casex g cl))
		       clauses)))))
(defmacro >casex (g cl)
  (let ((key (car cl))
	(rest (cdr cl)))
    (cond ((consp key) `((in ,g ,@key) ,@rest))
	  ((inq key t otherwise) `(t ,@rest))
	  (t (error "bad >case clause")))))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))
(defmacro till (test &body body)
  `(do ()
       (,test)
     ,@body))
(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
	  (,gstop ,stop))
	 ((> ,var ,gstop))
       ,@body)))


;; (do-tuples/o (x y) '(a b c d) (princ (list x y)))
;; -> (A B) (B C) (C D)
(defmacro do-tuples/o (parms source &body body)
  (if parms
      (let ((src (gensym)))
	`(prog ((,src ,source))
	       (mapc #'(lambda ,parms ,@body)
		     ,@(map0-n #'(lambda (n)
				   `(nthcdr ,n ,src))
			       (- (length source)
				  (length parms))))))))
;; (defmacro do-tuples/c (parms source &body body)
;; (defun dt-args (len rest src)

(defmacro mvdo* (parm-cl test-cl &body body)
  (mvdo-gen parm-cl parm-cl test-cl body))
(defun mvdo-gen (binds rebinds test body)
  (if (null binds)
      (let ((label (gensym)))
	`(prog nil
	       ,label
	       (if ,(car test)
		   (return (progn ,@(cdr test))))
	       ,@body
	       ,@(mvdo-rebind-gen rebinds)
	       (go ,label)))
      (let ((rec (mvdo-gen (cdr binds) rebinds test body)))
	(let ((var/s (caar binds)) 
	      (expr (cadar binds)))
	  (if (atom var/s)
	      `(let ((,var/s ,expr)) ,rec)
	      `(multiple-value-bind ,var/s ,expr ,rec))))))
(defun mvdo-rebind-gen (rebinds)
  (cond ((null rebinds) nil)
	((< (length (car rebinds)) 3)
	 (mvdo-rebind-gen (cdr rebinds)))
	(t 
	 (cons (list (if (atom (caar rebinds))
			 'setq
		         'multiple-value-setq)
		     (caar rebinds)
		     (third (car rebinds)))
	       (mvdo-rebind-gen (cdr rebinds))))))

(defmacro mvpsetq (&rest args)
  (let* ((pairs (group args 2))
	 (syms (mapcar #'(lambda (p)
			   (mapcar #'(lambda (x) (gensym))
				   (mklist (car p))))
		       pairs)))
    (labels ((rec (ps ss)
		  (if (null ps)
		      `(setq 
			,@(mapcan #'(lambda (p s)
				      (shuffle (mklist (car p))
					       s))
				  pairs syms))
		      (let ((body (rec (cdr ps) (cdr ss))))
			(let ((var/s (caar ps))
			      (expr (cadar ps)))
			  (if (consp var/s)
			      `(multiple-value-bind ,(car ss) ,expr
				 ,body)
			      `(let ((,@(car ss) ,expr))
				 ,body)))))))
      (rec pairs syms))))
(defun shuffle (x y)
  (cond ((null x) y)
	((null y) x)
	(t (list* (car x) (car y)
		  (shuffle (cdr x) (cdr y))))))

(defmacro mvdo (binds (test &test result) &body body)
  (let ((label (gensym))
	(temps (mapcar #'(lambda (b)
			   (if (listp (car b))
			       (mapcar #'(lambda (x) (gensym))
				       (car b))
			       (gensym)))
		       binds)))
    `(let ,(mappend #'mklist temps)
       (mvpsetq ,@(mapcan #'(lambda (b var)
			      (list var (cadr b)))
			  binds
			  temps))
       (prog ,(mapcar #'(lambda (b var) (list b var))
		      (mappend #'mklist (mapcar #'car binds))
		      (mappend #'mklist temps))
	     ,label
	     (if ,test
		 (return (progn ,@result)))
	     ,@body
	     (mvpsetq ,@(mapcan #'(lambda (b)
				    (if (third b)
					(list (car b)
					      (third b))))
				binds))
	     (go ,label)))))



;; chap 12
(defmacro allf (val &rest args)
  (with-gensyms (gval)
		`(let ((,gval ,val))
		   (setf ,@(mapcan #'(lambda (a) (list a gval))
				   args)))))
(defmacro nilf (&rest args) `(allf nil ,@args))
(defmacro tf (&rest args) `(allf t ,@args))
(defmacro toggle (&rest args)
  `(progn
     ,@(mapcar #'(lambda (a) `(toggle2 ,a))
	       args)))
(define-modify-macro toggle2 () not)

(define-modify-macro concf (obj) nconc)
(defun conc1f/function (place obj)
  (nconc place (list obj)))
(define-modify-macro conc1f (obj) conc1f/function)
(defun concnew/function (place obj &rest args)
  (unless (apply #'member obj place args)
    (nconc place (list obj))))
(define-modify-macro concnew (obj &rest args)
  concnew/function)



;; chap 13
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


;; chap 14
(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))		;use it to refer to the result
     (if it ,then-form ,else-form)))
(defmacro awhen (test-form &body body)
  `(aif ,test-form
	(progn ,@body)))
(defmacro awhile (expr &body body)
  `(do ((it ,expr ,expr))
       ((not it))
     ,@body))
(defmacro aand (&rest args)
  (cond ((null args) t)
	((null (cdr args)) (car args))
	(t `(aif ,(car args) (aand ,@(cdr args))))))
(defmacro acond (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
	    (sym (gensym)))
	`(let ((,sym ,(car cl1)))
	   (if ,sym
	       (let ((it ,sym)) 
		 ,@(cdr cl1))
	       (acond ,@(cdr clauses)))))))
(defmacro alambda (parms &body body)	;recursive lambda, refering itself as "self" 
  `(labels ((self ,parms ,@body))
     #'self))
(defmacro ablock (tag &rest args)
  `(block ,tag
     ,(funcall (alambda (args)
			(case (length args)
			  (0 nil)
			  (1 (car args))
			  (t '(let ((it ,(car args)))
				,(self (cdr args))))))
	       args)))

;; (defun count-instances (obj lists)
;;   "usage of alambda"
;;   (mapcar (alambda (list)
;; 		   (if list
;; 		       (+ (if (eq (car list) obj) 1 0)
;; 			  (self (cdr list)))
;; 		       0))
;; 	  lists))
		       
(defmacro aif2 (test &optional then else)
  (let ((win (gensym)))
    `(multiple-value-bind (it ,win) ,test
       (if (or it ,win) ,then ,else))))
(defmacro awhen2 (test &body body)
  `(aif2 ,test
	 (progn ,@body)))
(defmacro awhile2 (test &body body)
  (let ((flag (gensym)))
    `(let ((,flag t))
       (while ,flag
	 (aif2 ,test
	       (progn ,@body)
	       (setq ,flag nil))))))
(defmacro acond2 (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
	    (val (gensym))
	    (win (gensym)))
	`(multiple-value-bind (,val ,win) ,(car cl1)
	   (if (or ,val ,win)
	       (let ((it ,val)) ,@(cdr cl1))
	       (acond2 ,@(cdr clauses)))))))



(let ((g (gensym)))
  (defun read2 (&optional (str *standard-input*))
    (let ((val (read str nil g)))
      (unless (equal val g)
	(values val t)))))
(defmacro do-file (filename &body body)
  (let ((str (gensym)))
    `(with-open-file (,str ,filename)
		     (awhile2 (read2 ,str)
			      ,@body))))
(defun our-load (filename)
  (do-file filename (eval it)))


;; chap 15
(defmacro fn (expr) `#',(rbuild expr))
(defun rbuild (expr)
  (if (or (atom expr) (eq (car expr) 'lambda))
      expr
      (if (eq (car expr) 'compose)
	  (build-compose (cdr expr))
	  (build-call (car expr) (cdr expr)))))
(defun build-call (op fns)
  (let ((g (gensym)))
    `(lambda (,g)
       (,op ,@(mapcar #'(lambda (f)
			  `(,(rbuild f) ,g))
		      fns)))))
(defun build-compose (fns)
  (let ((g (gensym)))
    `(lambda (,g)
       ,(labels ((rec (fns)
		      (if fns
			  `(,(rbuild (car fns))
			    ,(rec (cdr fns)))
			  g)))
	  (rec fns)))))

(defmacro alrec (rec &optional base)
  "cltl2 version"
  (let ((gfn (gensym)))
    `(lrec #'(lambda (it ,gfn)
	       (symbol-macrolet ((rec (funcall ,gfn)))
		 ,rec))
	   ,base)))
(defmacro on-cdrs (rec base &rest lsts)
  `(funcall (alrec ,rec #'(lambda () ,base)) ,@lsts))

(defconstant unforced (gensym))
(defstruct delay forced closure)
(defmacro delay (expr)
  (let ((self (gensym)))
    `(let ((,self (make-delay :forced unforced)))
       (setf (delay-closure ,self)
	     #'(lambda ()
		 (setf (delay-forced ,self) ,expr)))
       ,self)))
(defun force (x)
  (if (delay-p x)
      (if (eq (delay-forced x) unforced)
	  (funcall (delay-closure x))
	  (delay-forced x))
      x))


;; chap 16
(defmacro abbrev (short long)
  `(defmacro ,short (&rest args)
     `(,',long ,@args)))
(defmacro abbrevs (&rest names)
  `(progn
     ,@(mapcar #'(lambda (pair)
		   `(abbrev ,@pair))
	       (group names 2))))

(defmacro propmacro (propname)
  `(defmacro ,propname (obj)
     `(get ,obj ',',propname)))
(defmacro propmacros (&rest props)
  `(progn
     ,@(mapcar #'(lambda (p) `(propmacro ,p))
	       props)))


;; chap 17
;; (set-macro-character #\'
;; 		     #'(lambda (stream char)
;; 			 (declare (ignore char))
;; 			 (list 'quote (read stream t nil t))))

;; chap 18
(defmacro dbind (pat seq &body body)
  (let ((gseq (gensym)))
    `(let ((,gseq ,seq))
       ,(dbind-ex (destruc pat gseq #'atom) body))))
(defun destruc (pat seq &optional (atom? #'atom) (n 0))
  (if (null pat)
      nil
      (let ((rest (cond ((funcall atom? pat) pat)
			((eq (car pat) '&rest) (cadr pat))
			((eq (car pat) '&body) (cadr pat))
			(t nil))))
	(if rest
	    `((,rest (subseq ,seq ,n)))
	    (let ((p (car pat))
		  (rec (destruc (cdr pat) seq atom? (1+ n))))
	      (if (funcall atom? p)
		  (cons `(,p (elt ,seq ,n))
			rec)
		  (let ((var (gensym)))
		    (cons (cons `(,var (elt ,seq ,n))
				(destruc p var atom?))
			  rec))))))))
(defun dbind-ex (binds body)
  (if (null binds)
      `(progn ,@body)
      `(let ,(mapcar #'(lambda (b)
			 (if (consp (car b))
			     (car b)
			     b))
		     binds)
	 ,(dbind-ex (mapcan #'(lambda (b)
				(if (consp (car b))
				    (cdr b)))
			    binds)
		    body))))

(defmacro with-matrix (pats ar &body body)
  (let ((gar (gensym)))
    `(let ((,gar ,ar))
       (let ,(let ((row -1))
	       (mapcan 
		#'(lambda (pat)
		    (incf row)
		    (let ((col -1))
		      (mapcar #'(lambda (p)
				  `(,p (aref ,gar 
					     ,row
					     ,(incf col))))
			      pat)))
		pats))
	 ,@body))))
(defmacro with-array (pat ar &body body)
  (let ((gar (gensym)))
    `(let ((,gar ,ar))
       (let ,(mapcar #'(lambda (p)
			 `(,(car p) (aref ,gar ,@(cdr p))))
		     pat)
	 ,@body))))
(defmacro with-struct ((name . fields) struct &body body)
  (let ((gs (gensym)))
    `(let ((,gs ,struct))
       (let ,(mapcar #'(lambda (f)
			 `(,f (,(symb name f) ,gs)))
		     fields)
	 ,@body))))
(defmacro with-places (pat seq &body body)
  (let ((gseq (gensym)))
    `(let ((,gseq ,seq))
       ,(wplac-ex (destruc pat gseq #'atom) body))))
(defun wplac-ex (binds body)
  (if (null binds)
      `(progn ,@body)
      `(symbol-macrolet ,(mapcar #'(lambda (b)
				     (if (consp (car b))
					 (car b)
				         b))
				 binds)
	 ,(wplac-ex (mapcan #'(lambda (b)
				(if (consp (car b))
				    (cdr b)))
			    binds)
		    body))))


	 

(defun match (x y &optional binds)
  (acond2
   ((or (eql x y) (eql x '_) (eql y '_)) (values binds t))
   ((binding x binds) (match it y binds))
   ((binding y binds) (match x it binds))
   ((varsym? x) (values (cons (cons x y) binds) t))
   ((varsym? y) (values (cons (cons y x) binds) t))
   ((and (consp x) (consp y) (match (car x) (car y) binds))
    (match (cdr x) (cdr y) it))
   (t (values nil nil))))
(defun varsym? (x)
  (and (symbolp x) (eq (char (symbol-name x) 0) #\?)))
(defun binding (x binds)
  (labels ((recbind (x binds)
		    (aif (assoc x binds)
			 (or (recbind (cdr it) binds)
			     it))))
    (let ((b (recbind x binds)))
      (values (cdr b) b))))
(defmacro if-match (pat seq then &optional else)
  `(aif2 (match ',pat ,seq)
	 (let ,(mapcar #'(lambda (v)
			   `(,v (binding ',v it)))
		       (vars-in then #'atom))
	   ,then)
	 ,else))
(defun vars-in (expr &optional (atom? #'atom))
  (if (funcall atom? expr)
      (if (var? expr) (list expr))
      (union (vars-in (car expr) atom?)
	     (vars-in (cdr expr) atom?))))
(defun var? (x)
  (and (symbolp x)
       (eq (char (symbol-name x) 0) #\?)))

(defmacro if-match (pat seq then &optional else)
  `(let ,(mapcar #'(lambda (v) `(,v ',(gensym)))
		 (vars-in pat #'simple?))
     (pat-match ,pat ,seq ,then ,else)))
(defmacro pat-match (pat seq then else)
  (if (simple? pat)
      (match1 `((,pat ,seq)) then else)
      (with-gensyms (gseq gelse)
		    `(labels ((,gelse () ,else))
		       ,(gen-match (cons (list gseq seq)
					 (destruc pat gseq #'simple?))
				   then
				   `(,gelse))))))
(defun simple? (x)
  (or (atom x) (eq (car x) 'quote)))
(defun gen-match (refs then else)
  (if (null refs)
      then
      (let ((then (gen-match (cdr refs) then else)))
	(if (simple? (caar refs))
	    (match1 refs then else)
	    (gen-match (car refs) then else)))))
(defun match1 (refs then else)
  (dbind ((pat expr) . rest) refs
	 (cond ((gensym? pat)
		`(let ((,pat ,expr))
		   (if (and (typep ,pat 'sequence)
			    ,(length-test pat rest))
		       ,then
		       ,else)))
	       ((eq pat '_) then)
	       ((var? pat)
		(let ((ge (gensym)))
		  `(let ((,ge ,expr))
		     (if (or (gensym? ,pat) (equal ,pat ,ge))
			 (let ((,pat ,ge)) ,then)
		         ,else))))
	       (t `(if (equal ,pat ,expr) ,then ,else)))))
(defun gensym? (s)
  (and (symbolp s) (not (symbol-package s))))
(defun length-test (pat rest)
  (let ((fin (caadar (last rest))))
    (if (or (consp fin) (eq fin 'elt))
	`(= (length ,pat) ,(length rest))
        `(> (length ,pat) ,(- (length rest) 2)))))


;; chap 19 query
(defun make-db (&optional (size 100))
  (make-hash-table :size size))
(defvar *default-db* (make-db))
(defun clear-db (&optional (db *default-db*))
  (clrhash db))
(defmacro db-query (key &optional (db '*default-db*))
  `(gethash ,key ,db))
(defun db-push (key val &optional (db *default-db*))
  (push val (db-query key db)))
(defmacro fact (pred &rest args)
  `(progn (db-push ',pred ',args)
	  ',args))

(defmacro with-answer (query &body body)
  (let ((binds (gensym)))
    `(dolist (,binds (interpret-query ',query))
       (let ,(mapcar #'(lambda (v)
			 `(,v (binding ',v ,binds)))
		     (vars-in query #'atom))
	 ,@body))))
(defun interpret-query (expr &optional binds)
  (case (car expr)
    (and (interpret-and (reverse (cdr expr)) binds))
    (or  (interpret-or  (cdr expr) binds))
    (not (interpret-not (cadr expr) binds))
    (t   (lookup (car expr) (cdr expr) binds))))
(defun interpret-and (clauses binds)
  (if (null clauses)
      (list binds)
      (mapcan #'(lambda (b)
		  (interpret-query (car clauses) b))
	      (interpret-and (cdr clauses) binds))))
(defun interpret-or (clauses binds)
  (mapcan #'(lambda (c)
	      (interpret-query c binds))
	  clauses))
(defun interpret-not (clauses binds)
  (if (interpret-query clauses binds)
      nil
      (list binds)))
(defun lookup (pred args &optional binds)
  (mapcan #'(lambda (x)
	      (aif2 (match x args binds) (list it)))
	  (db-query pred)))

(defmacro with-answer (query &body body)
  `(with-gensyms ,(vars-in query #'simple?)
		 ,(compile-query query  `(progn ,@body))))
(defun compile-query (q body)
  (case (car q)
    (and (compile-and (cdr q) body))
    (or  (compile-or  (cdr q) body))
    (not (compile-not (cadr q) body))
    (lisp `(if ,(cadr q) ,body))
    (t   (compile-simple q body))))
(defun compile-simple (q body)
  (let ((fact (gensym)))
    `(dolist (,fact (db-query ',(car q)))
       (pat-match ,(cdr q) ,fact ,body nil))))
(defun compile-and (clauses body)
  (if (null clauses)
      body
      (compile-query (car clauses)
		     (compile-and (cdr clauses) body))))
(defun compile-or (clauses body)
  (if (null clauses)
      nil
      (let ((gbod (gensym))
	    (vars (vars-in body #'simple?)))
	`(labels ((,gbod ,vars ,body))
	   ,@(mapcar #'(lambda (cl)
			 (compile-query cl `(,gbod ,@vars)))
		     clauses)))))
(defun compile-not (q body)
  (let ((tag (gensym)))
    `(if (block ,tag
	   ,(compile-query q `(return-from ,tag nil))
	   t)
	 ,body)))


;; chap 20 continuation
(defvar *actual-cont* #'values)
(define-symbol-macro *cont*
  *actual-cont*)

(defmacro =lambda (parms &body body)
  `#'(lambda (*cont* ,@parms) ,@body))
(defmacro =defun (name parms &body body)
  (let ((f (intern (concatenate 'string
				"="
				(symbol-name name)))))
    `(progn
       (defmacro ,name ,parms
	 `(,',f *cont* ,,@parms))
       (defun ,f (*cont* ,@parms) ,@body))))
(defmacro =bind (parms expr &body body)
  `(let ((*cont* #'(lambda ,parms ,@body)))
     ,expr))
(defmacro =values (&rest retvals)
  `(funcall *cont* ,@retvals))
(defmacro =funcall (fn &rest args)
  `(funcall ,fn *cont* ,@args))
(defmacro =apply (fn &rest args)
  `(apply ,fn *cont* ,@args))

(defun dft (tree)
  (cond ((null tree) nil)
	((atom tree) (princ tree))
	(t (dft (car tree))
	   (dft (cdr tree)))))
(defvar *saved* nil)
(=defun re-start ()
	(if *saved*
	    (funcall (pop *saved*))
	    (=values 'done)))
(=defun dft-node (tree)
	(cond ((null tree) (re-start))
	      ((atom tree) (=values tree))
	      (t (push #'(lambda () (dft-node (cdr tree)))
		       *saved*)
		 (dft-node (car tree)))))
(=defun dft2 (tree)
	(setq *saved* nil)
	(=bind (node) (dft-node tree)
	       (cond ((eq node 'done) (=values nil))
		     (t (princ node)
			(re-start)))))
  


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


	
  


;; chap 22 amb
(defparameter *paths* nil)
(defconstant failsym '@)

(defmacro choose (&rest choices)
  (if choices
      `(progn 
	 ,@(mapcar #'(lambda (c)
		       `(push #'(lambda () ,c) *paths*))
		   (reverse (cdr choices)))
	 ,(car choices))
      '(fail)))
(defmacro choose-bind (var choices &body body)
  `(cb #'(lambda (,var) ,@body) ,choices))
(defun cb (fn choices)
  (if choices
      (progn 
	(if (cdr choices)
	    (push #'(lambda () (cb fn (cdr choices)))
		  *paths*))
	(funcall fn (car choices)))
      (fail)))
(defun fail ()
  (if *paths*
      (funcall (pop *paths*))
      failsym))

;; (defun do2 (x)
;;   (choose (+ x 2) (* x 2) (expt x 2)))
;; -->
;; (defun do2 (x)
;;   (progn 
;;     (push #'(lambda () (expt x 2)) *paths*)
;;     (push #'(lambda () (* x 2)) *paths*)
;;     (+ x 2)))

;; (=defun two-numbers ()
;; 	(choose-bind n1 '(0 1 2 3 4 5)
;; 		     (choose-bind n2 '(0 1 2 3 4 5)
;; 				  (=values n1 n2))))
;; (=defun parlor-trick (sum)
;; 	(=bind (n1 n2) (two-numbers)
;; 	       (if (= (+ n1 n2) sum)
;; 		   `(the sum of ,n1 ,n2)
;; 		   (fail))))

;; (=defun descent (n1 n2)
;; 	(cond ((eq n1 n2) (=values (list n2)))
;; 	      ((kids n1) (choose-bind n (kinds n1)
;; 				      (=bind (p) (descent n n2)
;; 					     (=values (cons n1 p)))))
;; 	      (t (fail))))



;; chap 23 ATN
;; TODO
(defmacro defnode (name &rest arcs)
  `(=defun ,name (pos regs) (choose ,@arcs)))
(defmacro down (sub next &rest cmds)
  `(=bind (* pos regs) (,sub pos (cons nil regs))
	  (,next pos ,(compile-cmds cmds))))

;; chap 24 prolog





;; chap 25 CLOS

;; (defun rget (obj prop)
;;   (some2 #'(lambda (a) (gethash prop a))
;; 	 (get-ancestors obj)))
(defun get-ancestors (obj)
  (labels ((getall (x)
		   (append (list x)
			   (mapcan #'getall
				   (gethash 'parent x)))))
    (stable-sort (delete-duplicates (getall obj))
		 #'(lambda (x y)
		     (member y (gethash 'parents x))))))
(defun some2 (fn lst)
  (if (atom lst)
      nil
      (multiple-value-bind (val win) (funcall fn (car lst))
	(if (or val win)
	    (values val win)
	    (some2 fn (cdr lst))))))

(defun obj (&rest parents)
  (let ((obj (make-hash-table)))
    (setf (gethash 'parents obj) parents)
    (ancestors obj)
    obj))
(defun ancestors (obj)
  (or (gethash 'ancestors obj)
      (setf (gethash 'ancestors obj)
	    (get-ancestors obj))))
(defun rget (obj prop)
  (some2 #'(lambda (a) (gethash prop a))
	 (ancestors obj)))
(defmacro defprop (name &optional meth?)
  `(progn 
     (defun ,name (obj &rest args)
       ,(if meth?
	    `(run-methods obj ',name args)
	    `(rget obj ',name)))
     (defsetf ,name (obj) (val)
       `(setf (gethash ',',name ,obj) ,val))))

(defstruct meth around before primary after)
(defmacro meth- (field obj)
  (let ((gobj (gensym)))
    `(let ((,gobj ,obj))
       (and (meth-p ,gobj)
	    (,(symb 'meth- field) ,gobj)))))
;; (defun run-methods (obj name args)
;;   (let ((meth (rget obj name)))
;;     (if meth
;; 	(apply meth obj args)
;;         (error "No ~A method for ~A." name obj))))
(defun run-methods (obj name args)
  (let ((pr1 (rget obj name :primary)))
    (if pr1
	(let ((ar (rget obj name :around)))
	  (if ar
	      (apply ar obj args)
	      (run-core-methods obj name args pr1)))
        (error "No primary ~A method for ~A." name obj))))
(defun run-core-methods (obj name args &optional pr1)
  (multiple-value-prog1
   (progn (run-befores obj name args)
	  (apply (or pr1 (rget obj name :primary))
		 obj args))
   (run-afters obj name args)))
(defun rget (obj prop &optional meth (skip 0))
  (some2 #'(lambda (a)
	     (multiple-value-bind (val win) (gethash prop a)
	       (if win
		   (case meth 
		     (:around (meth- around val))
		     (:primary (meth- primary val))
		     (t (values val win))))))
	 (nthcdr skip (ancestors obj))))
(defun run-befores (obj prop args)
  (dolist (a (ancestors obj))
    (let ((bm (meth- before (gethash prop a))))
      (if bm 
	  (apply bm obj args)))))
(defun run-afters (obj prop args)
  (labels ((rec (lst)
		(when lst
		  (rec (cdr lst))
		  (let ((am (meth- after
				   (gethash prop (car lst)))))
		    (if am (apply am (car lst) args))))))
    (rec (ancestors obj))))

(defmacro defmeth ((name &optional (type :primary))
		    obj parms &body body)
  (let ((gobj (gensym)))
    `(let ((,gobj ,obj))
       (defprop ,name t)
       (unless (meth-p (gethash ',name ,gobj))
	 (setf (gethash ',name ,gobj) (make-meth)))
       (setf (,(symb 'meth- type) (gethash ',name ,gobj))
	     ,(build-meth name type gobj parms body)))))
(defun build-meth (name type gobj parms body)
  (let ((grags (gensym)))
    '#`(lambda (&rest ,gargs)
	 ;; TODO
	 )))