;;; -*- Mode: LISP; Fonts: MEDFNB -*-

(defmodel simple-lisp
  (:default-module MAIN)
  (:module MAIN :lisp-source "oz:<robbins.test>simple"))

(defmodel simple-c
  (:default-module MAIN)
  (:module MAIN c-source "oz:<robbins.test>simple"))

(defmodel circular-calls-lisp
  (:default-pathname "oz:<robbins.test>")
  (:default-module A)
  (:module A :lisp-source "circular-a")
  (:module B :lisp-source "circular-b")
  (:calls (A B) (A B)))

(defmodel circular-calls-c
  (:default-pathname "oz:<robbins.test>")
  (:default-module A)
  (:module A c-source "circular-a")
  (:module B c-source "circular-b")
  (calls (A B) (A B)))

(defmodel tiny-comp-lisp
  (:default-pathname "oz:<robbins.test>")
  (:default-module PARSER)
  (:module DEFS :lisp-source "definitions")
  (:module PARSER :lisp-source "parser")
  (:module GENERATOR :lisp-source "codegen")
  (:module LIBRARY :lisp-source "library")
  (:macro-calls (PARSER GENERATOR) DEFS)
  (:calls PARSER (LIBRARY GENERATOR))
  (:calls GENERATOR LIBRARY))

(defmodel tiny-comp-c
  (:default-pathname "oz:<robbins.test>")
  (:default-module PARSER)
  (:module DEFS c-source "definitions")
  (:module PARSER yacc-grammar "parser")
  (:module GENERATOR c-source "codegen")
  (:module LIBRARY c-object "library")
  (includes (PARSER GENERATOR) DEFS)
  (calls PARSER (LIBRARY GENERATOR))
  (calls GENERATOR LIBRARY))

(defmodel script-test
  (:default-pathname "oz:<robbins.test>")
  (:default-module MAIN)
  (:module MAIN shell-script "a")
  (:module COMMAND-1 c-source "b")
  (:module COMMAND-2 c-source "c")
  (:module SUPPORT c-source "d")
  (:module LOW c-source "e")
  (:module DEFS c-source "f")
  (calls MAIN (COMMAND-1 COMMAND-2))
  (calls (COMMAND-1 COMMAND-2) SUPPORT)
  (calls SUPPORT LOW)
  (includes LOW DEFS))
