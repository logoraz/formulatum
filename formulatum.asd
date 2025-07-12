(defsystem "formulatum"
  :description "Formulation Chemistry Software Solution"
  :author "Erik P Almaraz <erikalmaraz@fastmail.com>"
  :license "Apache-2.0"
  :version (:read-file-form "version.sexp" :at (0 1))
  :depends-on ("bordeaux-threads"
               "lparallel"
               "closer-mop"
               "mito"
               "osicat"
               "micros"
               "cl-cffi-gtk4")
  ;; Map of System Hierarchy
  :serial t
  :components
  ((:module "source"
    :serial t
    :components
    ((:module "utils"
      :serial t
      :components
      ((:file "files")
       (:file "servers")
       (:file "shell")
       (:file "strings")))

     (:module "core"
      :serial t
      :components
      ((:file "database")))

     ;; Main Program
     (:file "formulatum" :depends-on ("utils" "core"))))

   (:module "frontends"
    :serial t
    :components
    ((:file "gtk4-tutorial"))))

  :in-order-to ((test-op (test-op "formulatum/tests")))
  ;; Simply build with ccl/sbcl via (asdf:make :formulatum)
  ;; (ccl:save-application #p"formulatum-ccl" :toplevel #'formulatum:main :prepend-kernel t)
  :build-operation "program-op"
  :build-pathname "formulatum-preexe"
  :entry-point "formulatum:main"
  :long-description "
An extensible chemical formula builder/editor with regulatory intelligence.")


(defsystem "formulatum/tests"
  :components
  ((:module "tests"
    :serial t
    :components
    ((:file "base")
     (:file "utils"))))
  :depends-on ("rove")
  :perform (test-op (o c)
                    (unless (symbol-call :rove :run c)
                      (error "Tests failed"))))


(defsystem "formulatum/docs"
  :depends-on ())

#+nil
(register-system-packages "bordeaux-threads" '(:bt :bt2 :bordeaux-threads-2))

#+nil
(register-system-packages "closer-mop" '(:c2mop :c2cl :c2cl-user))
