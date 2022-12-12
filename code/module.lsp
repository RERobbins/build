;;; -*- Mode: LISP; Fonts: MEDFNB; Package: BUILD -*-
;;;
;;;        MODULE
;;;

(defflavor module
	(name			      ;symbol
	 type-name		      ;symbol
	 model			      ;containing model
	 citizens		      ;(pathname ...)
	 (m-nodes nil)		      ;(m-node ...)
	 (request-tag nil)	      ;gensym
	 (l-refs nil)		      ;((ref-name module-name) ...)
	 (r-refs nil))		      ;((ref-name module-name) ...)
	;; M-nodes correspond to the names in citizens
	;; L-refs --> (ref self other-module) references-as-left-member
	;; R-refs --> (ref other-module self) references-as-right-member
	()
  :gettable-instance-variables
  :settable-instance-variables
  :initable-instance-variables)

;;;
;;;        DEFMODULE
;;;

(defun defmodule (name type-name model pathnames default-pathname)
  (make-instance 'module
		 :name name
		 :type-name type-name
		 :model model
		 :citizens
		  (parse-citizens type-name pathnames default-pathname)))

(defun parse-citizens (type-name pathnames default-pathname &aux temp-name)
  ;; It is assumed that all citizens of modules are files and
  ;; hence the presence of the canonical-type field is assumed here.
  (let ((canonical-type
	  (send (lookup-module-type type-name) :fs-canonical-type)))
    (mapcar
      #'(lambda (name)
	  (setq temp-name (m-node-name-parse name default-pathname))
	  (cond ((send temp-name :type) temp-name)
		(t (send temp-name :new-type canonical-type))))
      pathnames)))

(defun m-node-name-parse (string default-pathname)
  (fs:merge-pathnames string default-pathname))
;;;
;;;        MODULE-METHODS
;;;

(defmethod (module :print-self) (&rest args)
  (format (first args) "#<MODULE ~A ~A>" name type-name))

(defmethod (module :acquaintances) ()
  (remove-duplicates (append (mapcar #'cdr l-refs)
			     (mapcar #'cdr r-refs))))

(defmethod (module :add-l-ref) (reference-name module-name)
  (let ((reference (cons reference-name module-name)))
    (if (null (member reference l-refs)) (push reference l-refs))))

(defmethod (module :add-r-ref) (reference-name module-name)
  (let ((reference (cons reference-name module-name)))
    (if (null (member reference r-refs)) (push reference r-refs))))

(defmethod (module :create-m-nodes) (tag)
  ;; This method is to be used to create the m-nodes associated with
  ;; the citizens of a source module.  The :set-exists method should not
  ;; be sent to these m-nodes, this function handles the functions performed
  ;; by :set-exists properly for these "source" m-nodes.
  (let ((module-type (lookup-module-type type-name)))
    (setq m-nodes
	  (mapcar
	   #'(lambda (citizen) (create-m-nodes-aux citizen module-type self))
	   citizens)))
  (setq request-tag tag))

(defun create-m-nodes-aux (pathname type module)
  ;; Pathname is from the citizen list and may not specify version
  ;; type must be a module-type-object.
  (let (name
	creation-date
	file-info
	(ingredients nil)
	candidate-date)
    ;; The type of the node must support exact naming and creation-date lookup
    ;; at this time only files are valid sources.
    (cond ((send type :files-p)
	   (setq file-info (get-file-info pathname))
	   (setq name (file-info-name file-info))
	   (setq creation-date (file-info-date file-info)))
	  (t (error 'invalid-module-type :module-type type)))
    ;; Be careful to look for the node as part of the m-node-table
    ;; If found, copy the ingredients, if not, leave that attribute nil
    (setq candidate-date (lookup-creation-date name))
    (if (equal candidate-date creation-date)
	(setq ingredients (lookup-ingredients name)))
    (install-m-node name ingredients creation-date)
    (make-m-node name type module ingredients creation-date)))
;;;
;;;        INVALID-MODULE-TYPE
;;;

(defflavor invalid-module-type (module-type) (error)
  :initable-instance-variables)

(defmethod (invalid-module-type :report) (stream)
  (format stream
	  "MODULE TYPE ~A DOES NOT SUPPORT NAME//CREATION-DATE LOOKUP."
	  module-type))
