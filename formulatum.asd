(defsystem "formulatum"
  :description "Formulation Chemistry Software Solution"
  :author "Erik P Almaraz <erikalmaraz@fastmail.com>"
  :license "Apache-2.0"
  :version (:read-file-form "version.sexp" :at (0 1))
  :class :package-inferred-system
  ;; :defsystem-depends-on ("frml-asdf-system")
  :depends-on ("bordeaux-threads"
               "lparallel"
               "closer-mop"
               "formulatum/core/all")
  :in-order-to ((test-op (test-op "formulatum/tests")))
  :long-description "
An extensible chemical formula builder/editor with regulatory intelligence.")


(defsystem "formulatum/tests"
  :class :package-inferred-system
  :depends-on ("rove"
               "formulatum/tests/base")
  :perform (test-op (o c)
                    (unless (symbol-call :rove :run c)
                      (error "Tests failed"))))

(defsystem "formulatum/docs"
  :class :package-inferred-system
  :depends-on ())


(register-system-packages "bordeaux-threads" '(:bt :bt2 :bordeaux-threads-2))

(register-system-packages "closer-mop" '(:c2mop :c2cl :c2cl-user))

