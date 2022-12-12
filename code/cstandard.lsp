;;; -*- Mode: LISP; Fonts: MEDFNB; Package: USER -*-
;;;
;;;        DEFINITIONS FOR C
;;;

(define-module-type yacc-grammar :y)
(define-module-type c-source :c)
(define-module-type c-object :o)
(define-module-type c-execute :exe)
(define-module-type shell-script :script)

(define-process-type c-compile
    ((source c-source :single) (includes c-source :multiple))
    ((object c-object :single source))
    stream
    (format stream "~%Compile ~A" (pathname-minus-version source))
  (format stream "~%Compiling ~A" source)
  (c-compile source object))

(define-process-type c-load
    ((primary c-object :single) (secondary c-object :multiple))
    ((image c-execute :single primary))
    stream
    (format stream "~%Link: ~A ~{~%  ~A~}"
	    (pathname-minus-version primary)
	    (mapcar #'pathname-minus-version secondary))
  (format stream "~%Linking: ~A ~{~%  ~A~}" primary secondary)
  (c-load primary secondary image))

(define-process-type yacc
    ((grammar yacc-grammar :single))
    ((parser c-source :single grammar))
    stream
    (format stream "~%YACC ~A" (pathname-minus-version grammar))
  (format stream "~%YACCing ~A" grammar)
  (yacc grammar parser))

;;;
;;;        :COMPILE C-SOURCE
;;;

(define-request-handler (:compile c-source :pre) (source-node)
  (access* source-node ((source c-compile) object)))

(define-reference-handler ((includes c-source c-source) (:compile :left))
                          (including-node included-node)
  (let ((compile-process (access including-node ((source c-compile)))))
    (push included-node (access+ compile-process (includes)))
    (process-request :include+ included-node compile-process)))

;;;
;;;        :LOAD C-SOURCE
;;;

(define-request-handler (:load c-source :pre) (source-node)
  (process-request :compile source-node)
  (access* source-node ((source c-compile) object (primary c-load) image)))

(define-reference-handler ((calls c-source c-source) (:load :left))
			  (calling-node called-node)
  (let ((linking-process
	  (access calling-node ((source c-compile) object (primary c-load)))))
    (process-request :compile called-node)
    (push (access called-node ((source c-compile) object))
	  (access+ linking-process (secondary)))
    (process-request :load+ called-node linking-process)))

(define-reference-handler ((calls c-source c-object) (:load :left))
			  (calling-node called-node)
  (let ((linking-process
	  (access calling-node ((source c-compile) object (primary c-load)))))
    (push called-node (access+ linking-process (secondary)))
    (process-request :load+ called-node linking-process)))

;;;
;;;        :LOAD C-OBJECT
;;;

(define-request-handler (:load c-object :pre) (object-node)
  (access* object-node ((primary c-load) image)))

(define-reference-handler ((calls c-object c-source) (:load :left))
			  (calling-node called-node)
  (let ((linking-process (access calling-node ((primary c-load)))))
    (process-request :compile called-node)
    (push (access called-node ((source c-compile) object))
	  (access+ linking-process (secondary)))
    (process-request :load+ called-node linking-process)))

(define-reference-handler ((calls c-object c-object) (:load :left))
			  (calling-node called-node)
  (let ((linking-process (access calling-node ((primary c-load)))))
    (push called-node (access+ linking-process (secondary)))
    (process-request :load+ called-node linking-process)))

;;;
;;;        :INCLUDE+ C-SOURCE C-COMPILE
;;;

(define-reference-handler ((includes c-source c-source) (:include+ :left))
			  (ignore included-node including-process)
  (push included-node (access+ including-process (includes)))
  (process-request :include+ included-node including-process))

;;;
;;;        :LOAD+ C-SOURCE C-LOAD
;;;

(define-reference-handler ((calls c-source c-source) (:load+ :left))
			  (ignore called-node linking-process)
  (process-request :compile called-node)
  (push (access called-node ((source c-compile) object))
	(access+ linking-process (secondary)))
  (process-request :load+ called-node linking-process))

(define-reference-handler ((calls c-source c-object) (:load+ :left))
			  (ignore called-node linking-process)
  (push called-node (access+ linking-process (secondary)))
  (process-request :load+ called-node linking-process))

;;;
;;;        :LOAD+ C-OBJECT C-LOAD
;;;

(define-reference-handler ((calls c-object c-source) (:load+ :left))
			  (ignore called-node linking-process)
  (process-request :compile called-node)
  (push (access called-node ((source c-compile) object))
	(access+ linking-process (secondary)))
  (process-request :load+ called-node linking-process))

(define-reference-handler ((calls c-object c-object) (:load+ :left))
			  (ignore called-node linking-process)
  (push called-node (access+ linking-process (secondary)))
  (process-request :load+ called-node linking-process))

;;;
;;;        :YACC YACC-GRAMMAR
;;;

(define-request-handler (:yacc yacc-grammar :pre) (grammar-node)
  (access* grammar-node ((grammar yacc) parser)))

;;;
;;;        :COMPILE YACC-GRAMMAR
;;;

(define-request-handler (:compile yacc-grammar :pre) (grammar-node)
  (process-request :yacc grammar-node)
  (access* grammar-node ((grammar yacc) parser (source c-compile) object)))

(define-reference-handler ((includes yacc-grammar c-source) (:compile :left))
			  (including-node included-node)
  (let ((compile-process
	  (access including-node ((grammar yacc) parser (source c-compile)))))
    (push included-node (access+ compile-process (includes)))
    (process-request :include+ included-node compile-process)))

;;;
;;;        :LOAD YACC-GRAMMAR
;;;

(define-request-handler (:load yacc-grammar :pre) (grammar-node)
  (process-request :compile grammar-node)
  (access* grammar-node ((grammar yacc) parser
			 (source c-compile) object
			 (primary c-load) image)))

(define-reference-handler ((calls yacc-grammar c-source) (:load :left))
			  (calling-node called-node)
  (let ((linking-process (access calling-node ((grammar yacc) parser
					       (source c-compile) object
					       (primary c-load)))))
    (process-request :compile called-node)
    (push (access called-node ((source c-compile) object))
	  (access+ linking-process (secondary)))
    (process-request :load+ called-node linking-process)))

(define-reference-handler ((calls yacc-grammar c-object) (:load :left))
			  (calling-node called-node)
  (let ((linking-process (access calling-node ((grammar yacc) parser
					       (source c-compile) object
					       (primary c-load)))))
    (push called-node (access+ linking-process (secondary)))
    (process-request :load+ called-node linking-process)))

;;;
;;;        :COMPILE SHELL-SCRIPT
;;;

(define-reference-handler ((calls shell-script c-source) (:compile :left))
			  (ignore called-node)
  (process-request :compile called-node))

(define-reference-handler ((calls shell-script c-object) (:compile :left))
			  (ignore called-node)
  (process-request :compile called-node))

(define-reference-handler ((calls shell-script yacc-grammar) (:compile :left))
			  (ignore called-node)
  (process-request :compile called-node))

;;;
;;;        :LOAD SHELL-SCRIPT
;;;

(define-reference-handler ((calls shell-script c-source) (:load :left))
			  (ignore called-node)
  (process-request :load called-node))

(define-reference-handler ((calls shell-script c-object) (:load :left))
			  (ignore called-node)
  (process-request :load called-node))

(define-reference-handler ((calls shell-script yacc-grammar) (:load :left))
			  (ignore called-node)
  (process-request :load called-node))

;;; Most Lisp Machines do not know about C objects, Grammars. etc.
;;; The canonical type definitions below would be used to define them
;;; if a Lisp Machine were actually used to manipulate these objects.

(fs:define-canonical-type :y "Y")
(fs:define-canonical-type :c "C")
(fs:define-canonical-type :o "O")
(fs:define-canonical-type :exe "EXE")
(fs:define-canonical-type :script "SCRIPT")

;;;
;;; Definitions for C processing tools.
;;;

(defun c-compile (source-name object-name)
  (with-open-file (object-stream object-name :out)
    (format object-stream "C Object from ~A of ~\date\."
	    source-name
	    (get-creation-date source-name))))

(defun c-load (primary-name secondary-namelist image-name)
  (with-open-file (image-stream image-name :out)
    (format image-stream
	    "C Image for:  ~A of ~\date\~{~%  ~A of ~\date\~}"
             primary-name
	     (get-creation-date primary-name)
	     (mapcan #'(lambda (name) `(,name ,(get-creation-date name)))
		    secondary-namelist))))

(defun yacc (grammar-name parser-name)
  (with-open-file (parser-stream parser-name :out)
    (format parser-stream "C Parser from ~A of ~\date\."
	    grammar-name
	    (get-creation-date grammar-name))))

;;;
;;; Misc. utility functions
;;;

(defun pathname-minus-version (pathname) (send pathname :new-version nil))

(defun get-creation-date (name)
  (cdr (with-open-file (stream name) (send stream :info))))
