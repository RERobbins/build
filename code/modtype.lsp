;;; -*- Mode: LISP; Fonts: MEDFNB; Package: BUILD-*-
;;;
;;;      MODULE-TYPE
;;;

(defflavor module-type
  (name				      ;symbol
   extension			      ;string
   fs-canonical-type)		      ;valid canonical-type --> files
				      ;nil --> not files
  ()
  :gettable-instance-variables
  :settable-instance-variables
  :initable-instance-variables)

(defmacro define-module-type (name &optional fs-canonical-type)
  `(install-module-type
    (make-instance
      'module-type
      :name ',name
      :extension (if (null ,fs-canonical-type) (get-pname ',name))
      :fs-canonical-type ',fs-canonical-type)))

(defmethod (module-type :print-self) (&rest args)
  (format (first args) "#<MODULE-TYPE ~A>" name))

(defmethod (module-type :files-p) () fs-canonical-type)

(defmethod (module-type :get-name-type) ()
  (if (send self :files-p) fs-canonical-type extension))
