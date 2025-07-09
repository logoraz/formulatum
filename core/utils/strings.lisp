(defpackage :formulatum/core/utils/strings
  (:use :cl :uiop)
  #+or
  (:import-from :cl-interpol)
  #+or
  (:import-from :cl-ppcre)
  #+or
  (:import-from :local-time)
  (:export #:concat)
  (:documentation "String utilities."))
(in-package :formulatum/core/utils/strings)

;; String manipulation
(defun concat (&rest strings)
  "Shorthand for CONCATENATE specialized for strings."
  (apply #'concatenate 'string strings))


