;;; -*- Mode: LISP; Fonts: MEDFNB; Package: BUILD -*-
;;;
;;;        DEFINITIONS FOR LISP
;;;

(define-module-type :lisp-source :lisp)
(define-module-type :lisp-binary :bin)
(define-module-type :lisp-image)

(define-process-type :lisp-compile
    ((source :lisp-source :single) (definitions :lisp-image :multiple))
    ((binary :lisp-binary :single source))
    output-stream
    (format output-stream "~%Compile ~A" (pathname-minus-version source))
  (format output-stream "~%Compiling ~A" source)
  (compiler:compile-file source binary))

(define-process-type :lisp-load-bin
    ((binary :lisp-binary :single) (definitions :lisp-image :multiple))
    ((image :lisp-image :single binary))
    output-stream
    (format output-stream "~%Load    ~A" (pathname-minus-version binary))
  (format output-stream "~%Loading   ~A" binary)
  (si:load-binary-file binary nil t))

;;;
;;;        :COMPILE :LISP-SOURCE
;;;

(define-request-handler (:compile :lisp-source :pre) (source-node)
  (access* source-node ((source :lisp-compile) binary)))

(define-reference-handler ((:macro-calls :lisp-source :lisp-source) (:compile :left))
			  (calling-node called-node)
  (process-request :load called-node)
  (push (access called-node ((source :lisp-compile) binary
			     (binary :lisp-load-bin) image))
	(access+ calling-node ((source :lisp-compile) definitions))))

;;;
;;;        :LOAD :LISP-SOURCE
;;;

(define-request-handler (:load :lisp-source :pre) (source-node)
  (process-request :compile source-node)
  (access* source-node ((source :lisp-compile) binary
			(binary :lisp-load-bin) image)))

(define-reference-handler ((:calls :lisp-source :lisp-source) (:load :left))
			  (ignore called-node)
  (process-request :load called-node))

(define-reference-handler ((:top-level-calls :lisp-source :lisp-source) (:load :left))
			  (calling-node called-node)
  (process-request :load called-node)
  (push (access called-node ((source :lisp-compile) binary
			     (binary :lisp-load-bin) image))
	(access+ calling-node ((source :lisp-compile) binary
			       (binary :lisp-load-bin) definitions))))
