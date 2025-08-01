(defpackage :formulatum
  (:nicknames :frml)
  (:use :cl
   :utils/strings)
  (:export #:main
           #:simple-test
           #:test-utils)
  (:documentation "Main package of formulatum"))
(in-package :formulatum)


(defun simple-test (&optional (n 11))
  "Simple function for testing."
  (loop :for i :from 0 :below n
        :collect (list (format nil "list ~A" i)
                       (/ i n))))

(defun test-utils ()
  (concat "string-1" " " "string-2"))

(defun main ()
  "Main entry point for the executable."
  (format t "Hello from Common Lisp! Arguments: ~A~%" 'no-args)
  #+or
  (progn
    #+clisp (ext:exit)
    #+(and ecl clasp) (ext:quit)
    #+ccl (ccl:quit)
    #+sbcl (sb-ext:quit))
  (uiop:quit))
