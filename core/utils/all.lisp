(uiop:define-package :formulatum/core/utils/all
  (:nicknames :core/utils)
  (:use :cl)
  (:use-reexport :formulatum/core/utils/strings
                 :formulatum/core/utils/files
                 :formulatum/core/utils/shell
                 :formulatum/core/utils/servers)
  (:documentation "Core utilities interface"))
(in-package :formulatum/core/utils/all)
