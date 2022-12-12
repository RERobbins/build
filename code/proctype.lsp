;;; -*- Mode: LISP; Fonts: MEDFNB; Package: BUILD-*-
;;;
;;;      PROCESS-TYPE
;;;

(defflavor process-type
  (name				      ;symbol
   input-spec			      ;((role-name type-name arity &opt name-source) ...)
   output-spec			      ;((role-name type-name arity &opt name-source) ...)
   construction-function	      ;symbol
   description-function)	      ;symbol
  ()
  :settable-instance-variables
  :gettable-instance-variables
  :initable-instance-variables)

(defmacro define-process-type (name input-spec output-spec stream-var
			       describe-form &body construct-body)
  (let ((describe-name (gensym))
	(construct-name (gensym))
	(let-forms
	 (mapcar
	  #'(lambda (role-name)
	      `(,role-name (build::get-players-names p-node ',role-name)))
	  (mapcar #'car (append input-spec output-spec))))
	(stream-form `(,stream-var stream)))
    `(eval-when (load compile eval)
       (defun ,describe-name (p-node &optional (stream t))
	 (let ,(cons stream-form let-forms) ,describe-form))
       (defun ,construct-name (p-node &optional (stream t))
	 (let ,(cons stream-form let-forms) ,@construct-body))
       (install-process-type
	(make-instance 'process-type
		       :name ',name
		       :input-spec ',input-spec
		       :output-spec ',output-spec
		       :construction-function ',construct-name
		       :description-function ',describe-name)))))

(defun get-players-names (p-node role-name &aux names)
  (setq names
	(mapcar
	  #'(lambda (node) (send node :name))
	  (send p-node :role-players role-name)))
  (if (send (send p-node :type) :output-role-p role-name)
      (setq names (mapcar #'pathname-minus-version names)))
  (if (eq :single (send (send p-node :type) :role-arity role-name))
      (first names)
      names))
;;;
;;;        PROCESS-TYPE VIEWING METHODS
;;;

(defmethod (process-type :print-self) (&rest args)
  (format (first args) "#<PROCESS-TYPE ~A>" name))

(defmethod (process-type :role-type) (role)
  (lookup-module-type (send self :role-type-name role)))

(defmethod (process-type :role-type-name) (role)
  (second (assq role (append input-spec output-spec))))

(defmethod (process-type :role-arity) (role)
  (third (assq role (append input-spec output-spec))))

(defmethod (process-type :role-name-source) (role)
  (fourth (assq role (append input-spec output-spec))))

(defmethod (process-type :initial-input-list) ()
  (mapcar #'(lambda (role-spec) (ncons (first role-spec)))
	  input-spec))

(defmethod (process-type :initial-output-list) ()
  (mapcar #'(lambda (role-spec) (ncons (first role-spec)))
	  output-spec))

(defmethod (process-type :input-role-p) (role) (assq role input-spec))

(defmethod (process-type :output-role-p) (role) (assq role output-spec))
