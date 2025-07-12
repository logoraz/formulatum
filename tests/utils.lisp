(defpackage :tests/utils
  (:use :cl
        :rove
        :utils/strings
        :utils/shell)
  (:export )
  (:documentation "Test suite for utils"))
(in-package :tests/utils)

;; Let's first define the "easy" tests
(deftest concat-test
  (ok (string-equal (concat "1 " "2") "1 2")))

(deftest executable-find-test
  (ok (string-equal (executable-find "ocicl")
                    (uiop:native-namestring "~/.local/bin/ocicl"))))

(run-suite *package*)
