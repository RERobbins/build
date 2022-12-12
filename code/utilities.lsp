;;; -*- Mode: LISP; Fonts: MEDFNB; Package: BUILD -*-
;;;
;;;        MISC. UTILTIES
;;;

(defun print-list (list &optional (stream t)) (format stream "~{~%~A~}" list))

(defun remove-duplicates (list &aux (first (car list)) (rest (cdr list)))
  (if list (cons first (remove-duplicates (remove first rest)))))

(defun set-equal (set1 set2)
  ;; Two sets are equal if each element in one can be found in the other.
  ;; Equal is used for comparison.
  (let* ((set1 (remove-duplicates set1))
	 (set2 (remove-duplicates set2))
	 (intersection (intersection-equal set1 set2)))
    (and (equal (length set1) (length set2))
	 (equal (length set1) (length intersection)))))

(defun intersection-equal (&rest lists &aux result (tail (locf result)))
  ;; Identical to intersection except that equal is used for comparison.
  (do ((list (car lists) (cdr list))) ((null list))
    (do ((rest (cdr lists) (cdr rest)))
	((null rest)
	 (rplacd tail (ncons (car list)))
	 (setq tail (cdr tail)))
      (or (member (car list) (car rest))      ;Use member instead of memq
	  (return nil))))
  result)

(defun pathname-minus-version (pathname) (send pathname :new-version nil))

(defun wildcard-version-match (name1 name2)
  (eq (pathname-minus-version name1) (pathname-minus-version name2)))

(defun touch (filename &aux (pathname (fs:parse-pathname filename)))
  (fs:change-file-properties
    pathname
    nil
    :creation-date (get-time (send pathname :host))))

(defun get-time (host) (net:invoke-service-on-host :time host))

(defun get-file-info (name) (with-open-file (stream name) (send stream :info)))

(defun file-info-name (info-cell) (car info-cell))

(defun file-info-date (info-cell) (cdr info-cell))
;;;
;;;      TABLE ABSTRACTION
;;;

(defun make-table () (cons 'table nil))

(defun table-p (table) (eq (car table) 'table))

(defun print-table (table &optional (stream t))
  (cond ((table-p table) (print-list (cdr table) stream))))

(defun add-entry (key value table)
  (let ((contents (cdr table)))
    (cond ((table-p table)
	   (cond ((assoc key contents)
		  (rplacd (assoc key contents) value))
		 (t (rplacd table
			    (cons (cons key value) contents))))))))

(defun delete-entry (key table)
  (let ((contents (cdr table)))
    (if (table-p table) (delq (assoc key contents) table))))

(defun get-value (key table)
  (cond ((table-p table) (cdr (assoc key (cdr table))))))

(defun key-defined (key table)
  (cond ((table-p table) (assoc key (cdr table)))))

(defun keys (table) (cond ((table-p table) (mapcar #'car (cdr table)))))

(defun elements (table) (cond ((table-p table) (mapcar #'cdr (cdr table)))))
