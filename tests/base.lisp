(defpackage :tests/base
  (:use :cl
        :rove
        :formulatum)
  (:export )
  (:documentation "Base testing suite"))
(in-package :tests/base)

(deftest example-test
  (ok (= 1 1)))

(run-suite *package*)
