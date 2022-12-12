;;; -*- Mode: LISP; Fonts: MEDFNB; Package: BUILD-TEST -*-

;;; This file would contain a special form that would expand into the parser.

(defmacro tiny-comp-parser-macro ()
  '(list 'tiny-comp-parser *tiny-comp-definitions*))

(defun tiny-comp-parser ()
  (list (tiny-comp-parser-macro)
	(tiny-comp-library)
	(tiny-comp-codegen)))
 
