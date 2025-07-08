(defsystem "formulatum"
  :description "Formulation Chemistry Software Solution"
  :author "Erik P Almaraz <erikalmaraz@fastmail.com>"
  :license "Apache-2.0"
  :version (:read-file-form "version.sexp" :at (0 1))
  :class :package-inferred-system
  :depends-on ("bordeaux-threads"
               "lparallel"
               "closer-mop"
               "formulatum/core/all")
  :in-order-to ((test-op (test-op "formulatum/tests")))
  ;; Simply build with ccl/sbcl via (asdf:make :formulatum)
  ;; (ccl:save-application #p"formulatum-ccl" :toplevel #'formulatum:main :prepend-kernel t)
  :build-operation "program-op"
  :build-pathname "formulatum-preexe"
  :entry-point "formulatum:main"
  :long-description "
An extensible chemical formula builder/editor with regulatory intelligence.")


(defsystem "formulatum/tests"
  :class :package-inferred-system
  :depends-on ("rove"
               "formulatum/tests/base"
               "formulatum/tests/utils")
  :perform (test-op (o c)
                    (unless (symbol-call :rove :run c)
                      (error "Tests failed"))))


(defsystem "formulatum/docs"
  :class :package-inferred-system
  :depends-on ())


(register-system-packages "bordeaux-threads" '(:bt :bt2 :bordeaux-threads-2))

(register-system-packages "closer-mop" '(:c2mop :c2cl :c2cl-user))
