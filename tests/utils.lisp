(defpackage :formulatum/tests/utils
  (:use :cl
        :rove
        :formulatum/core/utils/base)
  (:export )
  (:documentation "Test suite for utils"))
(in-package :formulatum/tests/utils)

(deftest concat-test
  (ok (string-equal (concat "1 " "2") "1 2")))

(run-suite *package*)