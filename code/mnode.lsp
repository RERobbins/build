;;; -*- Mode: LISP; Fonts: MEDFNB; Package: BUILD-*-
;;;
;;;        M-NODE
;;;

(defflavor m-node
	(name			      ;pathname
	 type			      ;module-type
	 module			      ;containing module
	 (creator nil)		      ;p-node
	 (users nil)		      ;list of p-nodes
	 ingredients		      ;list of <name.creation-date> tuples
	 creation-date)		      ;date object
	()
  :gettable-instance-variables
  :settable-instance-variables
  :initable-instance-variables)

(defun make-m-node (name type &optional module ingredients creation-date)
  (let ((type (if (symbolp type) (lookup-module-type type) type)))
    (make-instance 'm-node
		   :name name
		   :type type
		   :module module
		   :ingredients ingredients
		   :creation-date creation-date)))

;;;
;;;        M-NODE VIEWING METHODS
;;;

(defmethod (m-node :print-self) (&rest args)
  (format (first args) "#<M-NODE ~A ~A>" (send type :name) name))

(defmethod (m-node :source-p) () (null creator))

(defmethod (m-node :goal-p) () (null users))

(defmethod (m-node :exists-p) ()
  (lookup-m-node-record name))

(defmethod (m-node :type-name) () (send type :name))

(defmethod (m-node :module) ()
  (if (send self :source-p) module
      (ferror nil "~A IS NOT A SOURCE M-NODE." self)))

(defmethod (m-node :model) ()
  (if (send self :source-p) (send module :model)
      (ferror nil "~A IS NOT A SOURCE M-NODE." self)))
(defmethod (m-node :l-refs) ()
  (if (send self :source-p) (send module :l-refs)
      (ferror nil "~A IS NOT A SOURCE M-NODE." self)))

(defmethod (m-node :r-refs) ()
  (if (send self :source-p) (send module :r-refs)
      (ferror nil "~A IS NOT A SOURCE M-NODE." self)))

(defmethod (m-node :ancestors) (&aux inputs)
  (cond ((send self :source-p) nil)
	(t (setq inputs (send creator :inputs))
	   (remove-duplicates
	     (append
	       inputs
	       (apply #'append
		      (mapcar
			#'(lambda (m-node) (send m-node :ancestors))
			inputs)))))))

(defmethod (m-node :descendants) (&key (role nil) &aux (children nil))
  (dolist (user users)
    (cond ((or (null role) (memq role (send user :roles self)))
	   (setq children (append children (send user :outputs))))))
  (cond ((null children) nil)
	(t (remove-duplicates
	     (append
	       children
	       (apply #'append
		      (mapcar
			#'(lambda(m-node) (send m-node :descendants))
			children)))))))

(defmethod (m-node :derive-ingredients) ()
  ;; Ingredient lists --> ((name.creation-date) (name.creation-date) ...)
  ;; Each source node will be represented in the ingredient list of an m-node
  (mapcar
    #'(lambda (m-node) (cons (send m-node :name) (send m-node :creation-date)))
    (subset #'(lambda (node) (send node :source-p)) (send self :ancestors))))

(defmethod (m-node :ingredients-changed) ()
  (cond ((not (send self :exists-p))
	 (error 'non-existant-module :module self))
	((send self :source-p) nil) 
	(t (not (set-equal ingredients (send self :derive-ingredients))))))
;;;
;;;        M-NODE MUTATION METHODS
;;;

(defmethod (m-node :set-exists) (&aux file-info)
  (setq ingredients (send self :derive-ingredients))
  ;; If a file then get truename and creation-date from the file-system
  ;; If not a file then the name is OK as is and the creation-date
  ;; is the current time from the host specified in name.
  (cond ((send type :files-p)
	 (setq file-info (get-file-info name))
	 (setq name (file-info-name file-info))
	 (setq creation-date (file-info-date file-info)))
	(t (setq creation-date (get-time (send name :host)))))
  (install-m-node name ingredients creation-date))

(defmethod (m-node :add-user) (user)
  (if (null (memq user users)) (push user users)))

(defmethod (m-node :del-user) (user) (setq users (remq user users)))

;;;
;;;        NON-EXISTANT-MODULE
;;;

(defflavor non-existant-module (module) (error)
  :initable-instance-variables)

(defmethod (non-existant-module :report) (stream)
  (format stream "MODULE ~A DOES NOT EXIST." module))
