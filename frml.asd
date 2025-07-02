(defsystem :frml
  :description "Short alias for formulatum"
  :author "Erik P Almaraz <erikalmaraz@fastmail.com>"
  :license "Apache-2.0"
  :version (:read-file-form "version.sexp" :at (0 1))
  :depends-on (:formulatum)
  :in-order-to ((test-op (test-op "formulatum"))))
