;;; -*- Mode: LISP; Fonts: MEDFNB; Package: BUILD-*-
;;;
;;;        P-NODE
;;;

(defflavor p-node
	(type			      ;process-type
	 inputs			      ;((role-name m-node m-node ...) ...)
	 outputs		      ;((role-name m-node m-node ...) ...)
	 (internal-name (gensym)))
	()
  :initable-instance-variables)

(defun make-p-node (type)
  (let ((process-type (if (symbolp type) (lookup-process-type type) type)))
    (make-instance
      'p-node
      :type process-type
      :inputs (send process-type :initial-input-list)
      :outputs (send process-type :initial-output-list))))
;;;
;;;        VIEWING METHODS
;;;

(defmethod (p-node :print-self) (&rest args)
  (format (first args) "#<P-NODE ~A ~A>" (send type :name) internal-name))

(defmethod (p-node :type) () type)

(defmethod (p-node :role-players) (role)
  (cdr (assq role (append inputs outputs))))

(defmethod (p-node :derive-m-node-name) (role)
  (let* ((source-role (send type :role-name-source role))
	 (source-m-node (first (send self :role-players source-role)))
	 (module-type (send type :role-type role)))
    (if (null source-m-node)
	(ferror nil "Can't synthesize name: ~A ~A." self role))
    (send (send source-m-node :name) :new-pathname
	  :version :newest
	  :type (send module-type :get-name-type))))

(defmethod (p-node :roles) (m-node)
  (gather-roles-played m-node (append inputs outputs)))

(defun gather-roles-played (m-node role-lists)
  (cond ((null role-lists) nil)
	((memq m-node (first role-lists))
	 (cons (first (first role-lists))
	       (gather-roles-played m-node (rest1 role-lists))))
	(t (gather-roles-played m-node (rest1 role-lists)))))

(defmethod (p-node :inputs) () (apply #'append (mapcar #'cdr inputs)))

(defmethod (p-node :outputs) () (apply #'append (mapcar #'cdr outputs)))

(defmethod (p-node :descendant-m-node-p) (test-m-node)
  ;;Returns non-nil if test-m-node descends from self
  (let* ((output-m-nodes (send self :outputs))
	 (output-descendants
	   (apply #'append
		  (mapcar #'(lambda (node) (send node :descendants))
			  output-m-nodes))))
    (memq test-m-node (append output-m-nodes output-descendants))))

(defmethod (p-node :ancestral-m-node-p) (test-m-node)
  ;;Returns non-nil if test-m-node is an ancestor of self
  (let* ((input-m-nodes (send self :inputs))
	 (input-ancestors
	   (apply #'append
		  (mapcar #'(lambda (node) (send node :ancestors))
			  input-m-nodes))))
    (memq test-m-node (append input-m-nodes input-ancestors))))
;;;
;;;        MUTATION METHODS
;;;

(defmethod (p-node :add-player) (role m-node)
  (verify-role role type)
  (let ((role-alist (cond ((assq role inputs)) ((assq role outputs)))))
    (cond ((memq m-node role-alist) t)
	  ((neq (send type :role-type role) (send m-node :type))
	   (error 'type-error :process-type type :role role :m-node m-node))
	  ((and (second role-alist) (eq :single (send type :role-arity role)))
	   (error 'role-filled :p-node self :role role))
	  (t (nconc role-alist (list m-node))))))     ; Careful use of nconc

(defun verify-role (role type)
  (cond ((send type :input-role-p role))
	((send type :output-role-p role))
	(t (error 'Unknown-role :type type :role role))))

(defmethod (p-node :del-player) (role m-node)
  (delq m-node (cond ((assq role inputs)) ((assq role outputs)))))

(defmethod (p-node :describe-process) (&optional (stream t))
  (funcall (send type :description-function) self stream))

(defmethod (p-node :process) (&optional (stream t))
  (funcall (send type :construction-function) self stream)
  (mapcar #'(lambda (m-node) (send m-node :set-exists)) (send self :outputs)))
;;;
;;;        UNKNOWN-ROLE
;;;

(defflavor unknown-role (type role) (error)
  :initable-instance-variables)

(defmethod (unknown-role :report) (stream)
  (format stream "TYPE ~A DOES NOT HAVE ROLE ~A." type role))

;;;
;;;        TYPE-ERROR
;;;

(defflavor type-error (process-type role m-node) (error)
  :initable-instance-variables)

(defmethod (type-error :report) (stream)
  (format stream
	  "TYPE ERROR~%ROLE ~A OF ~A HAS MODULE TYPE ~A~%~A HAS TYPE ~A."
	  role
	  (send process-type :name)
	  (send process-type :role-type role)
	  m-node
	  (send m-node :type)))

;;;
;;;        ROLE-FILLED
;;;

(defflavor role-filled (p-node role) (error)
  :initable-instance-variables)

(defmethod (role-filled :report) (stream)
  (format stream "ROLE ALREADY FILLED - ~A ~A." p-node role))
