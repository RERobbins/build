;;; -*- Mode: LISP; Fonts: MEDFNB; Package: BUILD-TEST -*-

;;; This file contains the code generator

(defmacro tiny-comp-codegen-macro ()
  '(list 'tiny-comp-codegen *tiny-comp-definitions*))

(defun tiny-comp-codegen ()
  (list (tiny-comp-codegen-macro) (tiny-comp-library)))
  
