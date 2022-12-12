;;; -*- Mode: LISP; Fonts: MEDFNB; Package: BUILD-*-
;;;
;;;        ACCESS FAMILY OF MACROS
;;;

(defmacro access (node path)
  `(first (access-aux ,node ',path nil nil)))

(defmacro access* (node path)
  `(first (access-aux ,node ',path t nil)))

(defmacro access+ (node path)
  `(access-aux ,node ',path nil t))

(defun access-aux (node path &optional create fanout)
  (cond ((and create fanout)
	 (ferror nil "CAN'T CREATE LINKS AND ALLOW FANOUT."))
	((null path) (ncons node))
	((eq 'm-node (typep node))
	 (access-from-m-node node path create fanout))
	((eq 'p-node (typep node))
	 (access-from-p-node node path create fanout))
	(t (ferror nil "~A NOT AN M-NODE OR P-NODE." node))))

(defun access-from-m-node (m-node path create fanout &aux next-step p-nodes)
  (setq next-step (first path))     ; (role process-type-name)
  (if (or (nlistp next-step) (not (= (length next-step) 2)))
      (ferror nil "BAD PATH COMPONENT: ~A." next-step))
  (setq p-nodes
	(find-p-nodes m-node (first next-step) (second next-step)))
  (selectq (length p-nodes)
    (0 (if create
	   (access-aux
	     (create-link-from-m-node m-node (first next-step) (second next-step))
	     (rest1 path) create fanout)))
    (1 (access-aux (first p-nodes) (rest1 path) create fanout))
    (t (if fanout
	   (apply #'append
		  (mapcar
		    #'(lambda (node) (access-aux node (rest1 path) create fanout))
		    p-nodes))
	   (ferror nil "FANOUT VIOLATION: ~A ~A." m-node next-step)))))
(defun access-from-p-node (p-node path create fanout &aux role m-nodes)
  (setq role (first path))
  (if (nsymbolp role) (ferror nil "BAD PATH COMPONENT: ~A." role))
  (if (and (not fanout)
	   (eq :multiple (send (send p-node :type) :role-arity role)))
      (ferror nil "FANOUT VIOLATION: ~A ~A." p-node role))
  (setq m-nodes (send p-node :role-players role))
  (selectq (length m-nodes)
    (0 (if create
	   (access-aux
	     (create-link-from-p-node p-node role)
	     (rest1 path) create fanout)))
    (1 (access-aux (first m-nodes) (rest1 path) create fanout))
    (t (apply #'append
	      (mapcar
	       #'(lambda (node) (access-aux node (rest1 path) create fanout))
	       m-nodes)))))

(defun create-link-from-m-node (m-node role process-type)
  (let ((new-p-node (make-p-node process-type)))
    (plays-role m-node role new-p-node)
    new-p-node))

(defun create-link-from-p-node (p-node role)
  (let* ((m-node-type (send (send p-node :type) :role-type role))
	 (m-node-name (send p-node :derive-m-node-name role))
	 (new-m-node  (make-m-node m-node-name m-node-type)))
    (plays-role new-m-node role p-node)
    new-m-node))

(defun find-p-nodes (m-node role p-node-type-name)
  (let ((candidates (send m-node :users))
	(p-node-type (lookup-process-type p-node-type-name)))
    (if (not (send m-node :source-p)) (push (send m-node :creator) candidates))
    (find-p-nodes-aux m-node role p-node-type candidates)))

(defun find-p-nodes-aux (m-node role p-node-type candidates)
  (cond ((null candidates) nil)
	((and (eq p-node-type (send (first candidates) :type))
	      (memq m-node (send (first candidates) :role-players role)))
	 (cons (first candidates)
	       (find-p-nodes-aux m-node role p-node-type (rest1 candidates))))
	(t (find-p-nodes-aux m-node role p-node-type (rest1 candidates)))))
;;;
;;;        SETF ACCESS FUNCTIONS ETC.
;;;

(defprop access ((access node path).(setf-access node path si:val)) setf)

(defprop access* ((access* node path).(setf-access node path si:val)) setf)

(defprop access+ ((access+ node path).(setf-access+ node path si:val)) setf)

(defmacro setf-access (root-node path end-node)
  `(setf-access-aux ,root-node ',path ,end-node))

(defmacro setf-access+ (root-node path end-list)
  `(setf-access+-aux ,root-node ',path ,end-list))

(defun setf-access-aux (root-node path end-node)
  (cond ((null path) (ferror nil "CAN'T SETF ACCESS WITH NULL PATH."))
	((= 1 (length path)) (setf-link root-node (first path) end-node))
	(t (setf-access-aux
	     (first (access-aux root-node (ncons (first path)) t nil))
	     (rest1 path)
	     end-node))))

(defun setf-access+-aux (root-node path node-list)
  (cond ((null path) (ferror nil "CAN'T SETF ACCESS+ WITH NULL PATH."))
	((= 1 (length path)) (setf-link-list root-node (first path) node-list))
	(t (setf-access+-aux
	     (first (access-aux root-node (ncons (first path)) t nil))
	     (rest1 path)
	     node-list))))

(defun setf-link (root-node step end-node)
  (let* ((role (if (listp step) (first step) step))
	 (m-node (if (eq 'm-node (typep root-node)) root-node end-node))
	 (p-node (if (eq 'p-node (typep root-node)) root-node end-node))
	 (current-player (access-aux p-node (ncons role))))
    (cond ((eq current-player m-node))
	  (current-player
	   (unplays-role current-player role p-node)
	   (plays-role m-node role p-node))
	  (t (plays-role m-node role p-node)))))

(defun setf-link-list (root-node step node-list)
  (if (eq 'm-node (typep root-node))
      (setf-list-from-m-node root-node step node-list)
      (setf-list-from-p-node root-node step node-list)))
(defun setf-list-from-m-node (m-node step p-node-list &aux (role (first step)))
  (let* ((current-p-nodes (access-aux m-node (ncons step) nil t))
	 (ignore-list (intersection p-node-list current-p-nodes)))
    (dolist (p-node current-p-nodes)
      (if (not (memq p-node ignore-list)) (unplays-role m-node role p-node)))
    (dolist (p-node p-node-list)
      (if (not (memq p-node ignore-list)) (plays-role m-node role p-node)))))

(defun setf-list-from-p-node (p-node role m-node-list)
  (let* ((current-m-nodes (access-aux p-node (ncons role) nil t))
	 (ignore-list (intersection m-node-list current-m-nodes)))
    (dolist (m-node current-m-nodes)
      (if (not (memq m-node ignore-list)) (unplays-role m-node role p-node)))
    (dolist (m-node m-node-list)
      (if (not (memq m-node ignore-list)) (plays-role m-node role p-node)))))
;;;
;;;        PLAYS-ROLE
;;;

(defun plays-role (m-node role p-node &aux (p-type (send p-node :type)))
  (cond ((send p-type :input-role-p role)
	 (cond ((send p-node :descendant-m-node-p m-node)
		(error 'circular-construction
		       :m-node m-node :role role :p-node p-node))
	       (t (send p-node :add-player role m-node)
		  (send m-node :add-user p-node))))
	((send p-type :output-role-p role)
	 (cond ((send p-node :ancestral-m-node-p m-node)
		(error 'circular-construction
		       :m-node m-node :role role :p-node p-node))
	       (t (send p-node :add-player role m-node)
		  (send m-node :set-creator p-node))))
	(t (error 'unknown-role :type p-type :role role))))

(defun unplays-role (m-node role p-node &aux (p-type (send p-node :type)))
  (if (memq m-node (send p-node :role-players role))
      (cond ((send p-type :input-role-p role)
	     (send m-node :del-user p-node)
	     (send p-node :del-player role m-node))
	    ((send p-type :output-role-p role)
	     (send m-node :set-creator nil)
	     (send p-node :del-player role m-node)))))

;;;
;;;        CIRCULAR-CONSTRUCTION
;;;

(defflavor circular-construction (m-node role p-node) (error)
  :initable-instance-variables)

(defmethod (circular-construction :report) (stream)
  (format stream
	  "CIRCULAR CONSTRUCTION:~%~A CANNOT PLAY ROLE ~A IN ~A."
	  m-node role p-node))
;;;
;;;        DERIVE-PLAN
;;;

(defun derive-plan (goal-nodes)
  (remove-duplicates
    (apply #'append (mapcar #'derive-plan1 goal-nodes))))

(defun derive-plan1 (m-node &aux creator)
  ;; The following constraints ensured by BUILD
  ;; guarantee that this function returns nil when
  ;; a source node is passed to it.
  ;;     (not (send source-node :exists-p)) --> nil
  ;;     (send source-node :ingredients-changed) --> nil
  (cond ((or (not (send m-node :exists-p))   
	     (send m-node :ingredients-changed))
	 (setq creator (send m-node :creator))
	 (append 
	   (apply #'append
		  (mapcar #'derive-plan1 (send creator :inputs)))
	   (ncons creator)))))

;;;
;;;        BUILD
;;;

(defun build (plan &optional (stream t))
  (describe-construction plan stream)
  (tyo #\return stream)
  (tyo #\return stream)
  (if (y-or-n-p "Proceed? ") (construct-noconfirm plan))
  t)

(defun describe-construction (plan &optional (stream t))
  (dolist (node plan) (send node :describe-process stream))
  t)

(defun construct-noconfirm (plan &optional (stream t))
  (dolist (node plan) (send node :process stream))
  t)

(defun construct-quietly (plan)
  (dolist (node plan) (send node :process #'si:null-stream))
  t)
