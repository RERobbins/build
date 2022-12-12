;;; -*- Mode: LISP; Fonts: MEDFNB; Package: BUILD-TEST -*-

(defun a (item-list)
  (cond (item-list (b (cdr item-list)))
	(t "Finished in A.")))
