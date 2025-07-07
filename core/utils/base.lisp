(defpackage :formulatum/core/utils/base
  (:use :cl :uiop)
  #+or
  (:import-from :cl-interpol)
  #+or
  (:import-from :cl-ppcre)
  #+or
  (:import-from :local-time)
  (:import-from :osicat
                #:make-link
                #:file-kind)
  (:import-from :micros
                :create-server
                :stop-server)
  (:export #:start-micros
           #:stop-micros
           #:*shell-program*
           #:concat
           #:dir-pathname
           #:ensure-dir
           #:symlinkp
           #:create-symlink)
  (:documentation "Base utilities."))
(in-package :formulatum/core/utils/base)


;; String manipulation
(defun concat (&rest strings)
  "Shorthand for CONCATENATE specialized for strings."
  (apply #'concatenate 'string strings))


(defun executable-find (program)
  "Simple function to return path to PROGRAM"
  (remove #\newline 
          (uiop:run-program (list "which" program)
                            :output :string)))

;; Environment


;; Pathnames
(defun dir-pathname (pathspec)
  "Converts the non-wild pathname designator PATHSPEC to directory form."
  (uiop:ensure-directory-pathname pathspec))


;; directory creation/manipulation
;; Create directories (if they do no already exist)
(defun ensure-dir (pathspec &key (mode #o700))
  "Ensure directory in PATHSPEC exists"
  (let ((dir (dir-pathname pathspec)))
    (ensure-directories-exist dir :mode mode)))


;; Symlinks
(defun symlinkp (pathspec)
  "Test whether PATHSPEC is a symlink."
  (eq (osicat:file-kind pathspec) :symbolic-link))

(defun create-symlink (src link &key (dir nil))
  "Create a symlink for SRC to LINK."
  (let ((src-dir (dir-pathname src)))
    (if dir (osicat:make-link link :target src-dir)
        (osicat:make-link link :target src))))


;; Shell utilities (from StumpWM)
(defvar *shell-program* "/bin/sh"
  "The shell program used by @code{run-shell-command}.")


;;; source: stumpwm/wrappers.lisp
;; implementation specific --> sbcl
#+ (or)
(defun run-prog (prog &rest opts &key args output (wait t) &allow-other-keys)
  "Common interface to shell. Does not return anything useful."
  (remf opts :args)
  (remf opts :output)
  (remf opts :wait)
  (let ((env (sb-ext:posix-environ)))
    (apply #'sb-ext:run-program prog args :output (if output output t)
                                          :error t :wait wait :environment env opts)))
#+ (or)
(defun run-prog-collect-output (prog &rest args)
  "run a command and read its output."
  (with-output-to-string (s)
    (run-prog prog :args args :output s :wait t)))


;; TCP server
;; Only works in Lem with sbcl (ccl hangs)
(defvar *micros-port* 4005
  "Default Micros server port for Formulatum.")

(defun start-micros (&optional (micros-port *micros-port*))
  "Start a Micros server."
  (create-server :port micros-port :dont-close t)
  #+or
  (bt2:make-thread
   (lambda ()
     (create-server :port micros-port :dont-close t)))
  (format nil "Micros server started at port ~A" micros-port))

(defun stop-micros (&optional (micros-port *micros-port*))
  "Stop current Micros server."
  (stop-server micros-port)
  (format nil "Closing Micros server at port ~A" micros-port))


