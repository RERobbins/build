;;; -*- Mode: LISP; Fonts: MEDFNB; Package: BUILD -*-
;;;
;;;        SYSTEM TABLES FOR BUILD
;;;

;;;
;;;        *MODULE-TYPE-TABLE* <name|module-type>
;;;

(defvar *module-type-table* (make-table))

(defun install-module-type (module-type)
  (add-entry (send module-type :name) module-type *module-type-table*))

(defun lookup-module-type (name) (get-value name *module-type-table*))

;;;
;;;        *PROCESS-TYPE-TABLE* <name|process-type>
;;;

(defvar *process-type-table* (make-table))

(defun install-process-type (process-type)
  (add-entry (send process-type :name) process-type *process-type-table*))

(defun lookup-process-type (name) (get-value name *process-type-table*))

;;;
;;;        *MODEL-TABLE* <name|model>
;;;

(defvar *model-table* (make-table))

(defun install-model (model)
  (add-entry (send model :name) model *model-table*))

(defun lookup-model (name) (get-value name *model-table*))
;;;
;;;        *REFERENCE-TYPE-TABLE* <reference-signature|reference-handler-table>
;;;
				      
(defvar *reference-type-table* (make-table))

(defmacro define-reference (name)
  `(putprop ',name 'parse-assertion 'defmodel-function))

(defmacro define-reference-handler (((reference left-type right-type)
				     (request direction)) args &body body)
  (let* ((name (gensym)))
    `(eval-when (load compile eval)
       (defun ,name ,args ,@body)
       (let* ((signature
	       (make-reference-signature ',reference ',left-type ',right-type))
	      (handler-table (get-value signature *reference-type-table*)))
	 (cond ((null handler-table)
		(setq handler-table (make-table))
		(add-entry signature handler-table *reference-type-table*)))
	 (putprop ',reference 'parse-assertion 'defmodel-function) 
	 (add-entry ',request
		    (make-reference-handler signature ',request ,direction ',name)
		    handler-table)))))

(defun make-reference-handler (signature request direction name)
  (let ((reference (first signature))
	(left-type (second signature))
	(right-type (third signature)))
    (selectq direction
      (:left (cons name
		   (lookup-right-handler reference right-type left-type request)))
      (:right (cons (lookup-left-handler reference right-type left-type request)
		    name)))))

(defun lookup-left-handler (reference left-type right-type request)
  (let ((handler
	 (lookup-reference-handler reference left-type right-type request)))
    (if handler (car handler))))

(defun lookup-right-handler (reference left-type right-type request)
  (let ((handler
	 (lookup-reference-handler reference left-type right-type request)))
    (if handler (cdr handler))))

(defun make-reference-signature (name left-type right-type)
  (list name left-type right-type))

(defun lookup-reference-handler (reference left-type right-type request)
  (let* ((signature (make-reference-signature reference left-type right-type))
	 (handler-table (get-value signature *reference-type-table*)))
    (if handler-table (get-value request handler-table))))
;;;
;;;        *M-NODE-TABLE* <pathname|m-node-record>
;;;

;; *m-node-table* is initialized to nil.  Defmodel will ensure that it is
;; a table object before any model is loaded in.

(defvar *m-node-table* nil)

(defun node-table-initialized-p () (table-p *m-node-table*))

(defun init-m-node-table () (setq *m-node-table* (make-table)))

(defun forget (filename)
  (let* ((pathname (fs:parse-pathname filename))
	 (match-device (send pathname :device))
	 match-name)
    (dolist (m-node-name (m-node-names))
      (if match-device
	  (setq match-name m-node-name)
	  (setq match-name (send m-node-name :new-device nil)))
      (cond ((wildcard-version-match pathname match-name)
	     (if (probef m-node-name) (deletef m-node-name))
	     (delete-entry m-node-name *m-node-table*))))))

(defun m-node-names () (keys *m-node-table*))

(defun install-m-node (pathname ingredients creation-date)
  (add-entry
    pathname
    (make-m-node-record ingredients creation-date)
    *m-node-table*))

(defun lookup-ingredients (pathname)
  (let ((m-node-record (lookup-m-node-record pathname)))
    (if m-node-record (ingredients m-node-record))))

(defun lookup-creation-date (pathname)
  (let ((m-node-record (lookup-m-node-record pathname)))
    (if m-node-record (creation-date m-node-record))))

(defun lookup-m-node-record (pathname) (get-value pathname *m-node-table*))

(defun make-m-node-record (ingredients creation-date) 
  (cons ingredients creation-date))

(defun ingredients (m-node-record) (car m-node-record))

(defun creation-date (m-node-record) (cdr m-node-record))
;;;
;;;        *REQUEST-TABLE* <request-signature|request-handler>
;;;

(defvar *request-table* (make-table))

(defmacro define-request-handler ((request module-type pre-or-post) args &body body)
  (let* ((name (gensym)))
    `(eval-when (load compile eval)
       (defun ,name ,args ,@body)
       (add-request-handler ',request ',module-type ,pre-or-post ',name))))

(defun add-request-handler (request module-type pre-or-post name)
  (selectq pre-or-post
    (:pre 
     (add-entry
       (make-request-signature request module-type)
       (make-request-handler name (post-request-handler request module-type))
       *request-table*))
    (:post
     (add-entry
       (make-request-signature request module-type)
       (make-request-handler (pre-request-handler request module-type) name)
       *request-table*))))

(defun pre-request-handler (request module-type)
  (let* ((signature (make-request-signature request module-type))
	 (handler (get-value signature *request-table*)))
    (if handler (pre-assertion-function handler))))

(defun post-request-handler (request module-type)
  (let* ((signature (make-request-signature request module-type))
	 (handler (get-value signature *request-table*)))
    (if handler (post-assertion-function handler))))

(defun make-request-signature (request module-type) (cons request module-type))

(defun make-request-handler (pre post) (cons pre post))

(defun pre-assertion-function (request-handler) (car request-handler))

(defun post-assertion-function (request-handler) (cdr request-handler))
