

(defpackage #:onlisp.ai
  (:use #:cl #:onlisp))
(in-package :onlisp.ai)

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





