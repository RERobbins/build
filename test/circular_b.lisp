;;; -*- Mode: LISP; Fonts: MEDFNB; Package: BUILD-TEST -*-

(defun b (item-list)
  (cond (item-list (a (cdr item-list)))
	(t "Finished in B.")))
