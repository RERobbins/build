;;; -*- Mode: LISP; Fonts: MEDFNB; Package: BUILD-*-
;;;
;;;        MODEL
;;;

(defflavor model
	(name			      ;symbol
	(module-table (make-table))   ;table <symbol.module>
	(default-pathname (fs:user-homedir))   ;pathname
	(default-module-name nil)     ;symbol
	(request-tag nil)	      ;gensym
	(request-list nil))	      ;((request.module-name) ...)
	()
  :gettable-instance-variables
  :settable-instance-variables
  :initable-instance-variables)

(defmethod (model :print-self) (&rest args)
  (format (first args) "#<MODEL ~A>" name))

(defmethod (model :lookup-module) (module-name)
  (get-value module-name module-table))

(defmethod (model :add-module) (module)
  (add-entry (send module :name) module module-table))

(defmethod (model :goal-m-nodes) ()
  (let* ((modules (elements module-table))
	 (m-nodes
	   (apply #'append
		  (mapcar
		    #'(lambda (module)
			(conditional-get-m-nodes-complete request-tag module))
		    modules))))
    (remove-duplicates
      (subset #'(lambda (m-node) (send m-node :goal-p)) m-nodes))))

(defun conditional-get-m-nodes-complete (tag module)
  (cond ((eq tag (send module :request-tag))
	 (apply
	   #'append
	   (mapcar #'(lambda (m-node) (cons m-node (send m-node :descendants)))
		   (send module :m-nodes))))))
;;;
;;;        DEFMODEL
;;;

(defmacro defmodel (name &rest model-data)
  `(let* ((model (make-instance 'model :name ',name)))
     (if (not (node-table-initialized-p)) (install-m-node-table))
     (install-model model)
     (dolist (model-item ',model-data)
       (let ((function (get (car model-item) 'defmodel-function))
	     (data (cons model model-item)))
	 (apply function data)))
     model))

(defun install-m-node-table ()
  (let ((pathname (fs:merge-pathnames "m-node-table.build" (fs:user-homedir))))
    (if (probef pathname) (load-m-node-table pathname) (init-m-node-table))
    (push `(dump-m-node-table ',pathname) logout-list)))

(defprop :default-pathname parse-default-pathname defmodel-function)

(defprop :default-module parse-default-module defmodel-function)

(defprop :module parse-module defmodel-function)

(defun parse-default-pathname (model ignore pathname)
  (send model :set-default-pathname (fs:parse-pathname pathname)))

(defun parse-default-module (model ignore default-module-name)
  (send model :set-default-module-name default-module-name))

(defun parse-module (model ignore name type-name &rest citizens)
  (let ((default-pathname (send model :default-pathname)))
    (send model :add-module
	  (defmodule name type-name model citizens default-pathname))))

(defun parse-assertion (model reference-name left-names right-names)
  (let (left-module
	right-module)
    (if (symbolp left-names) (setq left-names (list left-names)))
    (if (symbolp right-names) (setq right-names (list right-names)))
    (dolist (left-name left-names)
      (setq left-module (send model :lookup-module left-name))
      (dolist (right-name right-names)
	(setq right-module (send model :lookup-module right-name))
	(send left-module :add-l-ref reference-name right-name)
	(send right-module :add-r-ref reference-name left-name)))
    reference-name))
