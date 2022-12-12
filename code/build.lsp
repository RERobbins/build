;;; -*- Mode: LISP; Fonts: MEDFNB; Package: BUILD -*-
;;;
;;;       BUILD-REQUEST
;;;

(defmacro build-request (model-name request &optional module-name (mode :normal))
  `(let ((model (lookup-model ',model-name))
	 (module-name ',module-name)
	 module
	 goal-nodes
	 plan)
     (if (null model) (ferror nil "NO MODEL NAMED ~A." ',model-name))
     (setq module-name (or module-name (send model :default-module-name)))
     (setq module (send model :lookup-module module-name))
     (if (null module) (ferror nil "NO MODULE NAMED ~A." module-name))
     (send model :set-request-tag (gensym))
     (send model :set-request-list nil)
     (check-module-m-nodes module)
     (dolist (m-node (send module :m-nodes))
       (process-request ',request m-node))
     (setq goal-nodes (send model :goal-m-nodes))
     (patch-non-specific-m-nodes goal-nodes)
     (setq plan (derive-plan goal-nodes))
     (if plan
	 (selectq ',mode
	   (:quiet (construct-quietly plan))
	   (:describe (describe-construction plan))
	   (:no-confirm (construct-noconfirm plan))
	   (:normal (build plan))
	   (t (ferror nil "UNKNOWN CONSTRUCTION MODE KEYWORD ~A." ',mode))))))

(defun process-request (request m-node &rest args &aux req-signature)
  (let* ((model (send m-node :model))
	 (args (copylist args))
	 (req-list (send model :request-list)))
    (setq req-signature `(,request ,m-node ,@args))
    (cond ((member req-signature req-list))
	  (t (send model :set-request-list (cons req-signature req-list))
	     (pre-request-process request m-node args)
	     (reference-process request m-node args)
	     (post-request-process request m-node args)))))

(defun pre-request-process (request m-node others)
  (let* ((m-node-type-name (send m-node :type-name))
	 (handler (pre-request-handler request m-node-type-name)))
    (if handler (apply handler `(,m-node ,@others)))))

(defun post-request-process (request m-node others)
  (let* ((m-node-type-name (send m-node :type-name))
	 (handler (post-request-handler request m-node-type-name)))
    (if handler (apply handler `(,m-node ,@others)))))

(defun reference-process (request m-node others)
  (process-l-refs request m-node others)
  (process-r-refs request m-node others))
(defun patch-non-specific-m-nodes (goal-nodes)
  (dolist (goal-node goal-nodes)
    (dolist (m-node (cons goal-node (send goal-node :ancestors)))
      (cond ((send m-node :exists-p)
	     (let ((name (send m-node :name)))
	       (send m-node :set-creation-date (lookup-creation-date name))
	       (send m-node :set-ingredients (lookup-ingredients name))))
	    (t (search-for-proper-m-node m-node))))))

(defun search-for-proper-m-node (m-node)
  (let ((ingredients (send m-node :derive-ingredients))
	(candidate-names (gather-similar-names m-node))
	candidate-ingredients)
    (dolist (name candidate-names)
      (setq candidate-ingredients (lookup-ingredients name))
      (cond ((set-equal ingredients candidate-ingredients)
	     (send m-node :set-name name)
	     (send m-node :set-ingredients (lookup-ingredients name))
	     (send m-node :set-creation-date (lookup-creation-date name))
	     (return nil))))))

(defun gather-similar-names (m-node)
  ;; Return a list of keys that differ from the name of m-node in version only
  (subset (let-closed ((name (send m-node :name)))
	    #'(lambda (key) (wildcard-version-match name key)))
	  (m-node-names)))

(defun process-l-refs (request l-m-node others)
  (let ((l-type (send l-m-node :type-name))
	(l-refs (send l-m-node :l-refs))
	(model (send l-m-node :model)))
    (dolist (l-ref l-refs)
      (let* ((ref (car l-ref))
	     (r-module (send model :lookup-module (cdr l-ref)))
	     (r-type (send r-module :type-name))
	     handler)
	(check-module-m-nodes r-module)
	(setq handler (lookup-left-handler ref l-type r-type request))
	(if handler
	    (dolist (r-m-node (send r-module :m-nodes))
	      (apply handler `(,l-m-node ,r-m-node ,@others))))))))
(defun process-r-refs (request r-m-node others)
  (let ((r-type (send r-m-node :type-name))
	(r-refs (send r-m-node :r-refs))
	(model (send r-m-node :model)))
    (dolist (r-ref r-refs)
      (let* ((ref (car r-ref))
	     (l-module (send model :lookup-module (cdr r-ref)))
	     (l-type (send l-module :type-name))
	     handler)
	(check-module-m-nodes l-module)
	(setq handler (lookup-right-handler ref l-type r-type request))
	(if handler
	    (dolist (l-m-node (send l-module :m-nodes))
	      (apply handler `(,l-m-node ,r-m-node ,@others))))))))

(defun check-module-m-nodes (module)
  (let* ((model (send module :model))
	 (request-tag (send model :request-tag)))
    (cond ((eq request-tag (send module :request-tag)))
	  (t (send module :create-m-nodes request-tag)))))
;;;
;;;        DUMP-M-NODE-TABLE
;;;

(defun dump-m-node-table (filename &optional (validate nil))
  (let* ((names (m-node-names))
	 (output-object nil))
    (if validate (setq names (subset #'validate-m-node names)))
    (setq output-object (create-dump-object names))
    (with-open-file (dump-stream filename :out)
      (prin1 output-object dump-stream))))

(defun validate-m-node (name &optional creation-date)
  (and (probef name)
       (equal (file-info-date (get-file-info name))
	      (if creation-date creation-date (lookup-creation-date name)))))

(defun create-dump-object (names) (mapcar #'unparse-table-item names))

(defun unparse-table-item (name)
  (cons (send name :string-for-printing)
	(unparse-m-node-record name)))

(defun unparse-m-node-record (name)
  (let ((ingredients (lookup-ingredients name))
	(creation-date (lookup-creation-date name)))
    (cons (unparse-ingredient-list ingredients)
	  (time:print-universal-time creation-date nil))))

(defun unparse-ingredient-list (ingredients)
  (mapcar #'unparse-ingredient ingredients))

(defun unparse-ingredient (ingredient)
  (let ((pathname (car ingredient))
	(creation-date (cdr ingredient)))
    (cons (send pathname :string-for-printing)
	  (time:print-universal-time creation-date nil))))
;;;
;;;        LOAD-M-NODE-TABLE
;;;

(defun load-m-node-table (filename &optional (validate t) (overwrite t))
  (let ((input-object nil))
    (with-open-file (read-stream filename :in)
      (setq input-object (read read-stream)))
    (if overwrite (init-m-node-table))
    (dolist (input-item input-object)
      (let* ((name (fs:parse-pathname (car input-item)))
	     (m-node-record (parse-m-node-record (cdr input-item))))
	(if (or (null validate) (validate-m-node name (creation-date m-node-record)))
	    (install-m-node
	      name
	      (ingredients m-node-record)
	      (creation-date m-node-record)))))))

(defun parse-m-node-record (m-node-record)
  (let ((ingredients (car m-node-record))
	(creation-date (cdr m-node-record)))
    (make-m-node-record (parse-ingredient-list ingredients)
			(time:parse-universal-time creation-date))))

(defun parse-ingredient-list (ingredients) (mapcar #'parse-ingredient ingredients))

(defun parse-ingredient (ingredient)
  (let ((pathname (car ingredient))
	(creation-date (cdr ingredient)))
    (cons (fs:parse-pathname pathname)
	  (time:parse-universal-time creation-date))))
