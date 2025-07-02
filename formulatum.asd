(defsystem "formulatum"
  :description "Formulation Chemist Software Solution"
  :author "Erik P Almaraz <erikalmaraz@fastmail.com>"
  :license "Apache-2.0"
  :version (:read-file-form "version.sexp" :at (0 1))
  :class :package-inferred-system
  :depends-on ("bordeaux-threads"
               "lparallel"
               "closer-mop"
               "formulatum/core/all")
  :in-order-to ((test-op (test-op "formulatum/test")))
  :long-description "
A chemical formula builder/editor tool with regulatory intelligence.")


(defsystem "formulatum/test"
  :depends-on ("fiveam"
               "formulatum/tests/all")
  :perform (test-op (op c)
                    (symbol-call :fiveam :run!
                                 (find-symbol* :root-suite :formulatum/test))))

(defsystem "formulatum/docs"
  :depends-on ())


(asdf:register-system-packages "formulatum/tests/all" '(:formulatum/test))

(register-system-packages "bordeaux-threads" '(:bt :bt2 :bordeaux-threads-2))

(register-system-packages "closer-mop" '(:c2mop :c2cl :c2cl-user))

(register-system-packages "fiveam" '(:5am))
