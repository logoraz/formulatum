(defsystem "formulatum"
  :description "Formulation Chemistry Software Solution"
  :author "Erik P Almaraz <erikalmaraz@fastmail.com>"
  :license "Apache-2.0"
  :version (:read-file-form "version.sexp" :at (0 1))
  :depends-on ("bordeaux-threads"
               "lparallel"
               "closer-mop"
               "fast-generic-functions"
               "osicat"
               "mito"
               "micros"
               #+sbcl
               "cl-cffi-gtk4"
               ;; Local Systems (aka libraries)
               )
  :components ;; Map of System
  ((:module "source"
    :components 
    ((:module "utils" ;; Establish first our toolbox
      :components 
      ((:file "macros")
       (:file "files")
       (:file "strings")                               
       (:file "shell")
       (:file "servers")))
     ;; Program Core Logic
     (:module "core"
      :depends-on ("utils")
      :components ((:file "database")))
     ;; Main Program
     (:file "formulatum" :depends-on ("utils" "core"))))
   ;; UI/X Frontends
   (:module "frontends"
    :components 
    (#+sbcl (:file "gtk4-tutorial"))))
  ;; Building (executables) & Testing
  :build-operation "program-op"
  :build-pathname "formulatum-preexe"
  :entry-point "formulatum:main"
  :in-order-to ((test-op (test-op "formulatum/tests")))
  :long-description "An extensible chemical formula builder/editor with 
regulatory intelligence.")

;; Register specific package symbols
(register-system-packages "bordeaux-threads" '(:bt :bt2 :bordeaux-threads-2))
(register-system-packages "closer-mop" '(:c2mop :c2cl :c2cl-user))


(defsystem "formulatum/docs"
  :depends-on ("formulatum" "3bmd" "colorize" "print-licenses")
  :components
  ((:module "docs"
    :components
    ((:file "formulatum-docs")))))


(defsystem "formulatum/libraries"
  :depends-on ())


(defsystem "formulatum/tests"
  :depends-on ("formulatum" "rove")
  :components
  ((:module "tests"
    :components
    ((:file "base")
     (:file "utils"))))
  :perform (test-op (o c)
                    (unless (symbol-call :rove :run c)
                      (error "Tests failed"))))
