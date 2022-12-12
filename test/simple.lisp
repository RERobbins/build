;;; -*- Mode: LISP; Fonts: MEDFNB; Package: BUILD-TEST -*-

(defun fib (n)
  (cond ((= 1 n) 1)
	(t (* n (fib (- n 1))))))
