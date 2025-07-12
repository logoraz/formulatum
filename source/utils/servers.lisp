(defpackage :utils/servers
  (:use :cl :uiop)
  (:import-from :micros
                :create-server
                :stop-server)
  (:export #:*micros-port*
           #:start-micros
           #:stop-micros)
  (:documentation "TCP Lisp Servers"))
(in-package :utils/servers)

;; TCP servers

;; Only works in Lem with sbcl (ccl hangs)
;; TODO: Identify & Report bug with Lem regarding this...
(defvar *micros-port* 4005
  "Default Micros server port for Formulatum.")

(defun start-micros (&optional (micros-port *micros-port*))
  "Start a Micros server."
  (create-server :port micros-port :dont-close t)
  (format nil "Micros server started at port ~A" micros-port))

(defun stop-micros (&optional (micros-port *micros-port*))
  "Stop current Micros server."
  (stop-server micros-port)
  (format nil "Closing Micros server at port ~A" micros-port))

