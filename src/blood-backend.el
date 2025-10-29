;;; blood-backend.el -*- lexical-binding: t; no-byte-compile: t; -*-
(llog! "Blood Backend")

(defvar blood--backend-active nil
  "The backend struct describing the default backend to use. comprised of :name, :require, :bootstrap and :actviator ")
(defconst blood--backend-default 'straight)

(defvar blood--registered-backends (make-hash-table))

(cl-defstruct blood--backend-s
  "The interface functions of a package management backend"
  (name      nil      :type symbol      :doc "the name of the backend")
  (requires  nil      :type list:symbol :doc "a list of package symbols this backend will require")
  (bootstrap nil      :type lambda      :doc "a list of (lamdba (profile)) -> nil, functions to configure the backend")
  (activator nil      :type lambda      :doc "(lambda (profile)) -> nil, activates the backend")
  (sync      nil      :type lambda      :doc "(lambda (packages)) -> nil, for the backend to install given packages")
  (clean     nil      :type lambda      :doc "(lambda ()) -> nil, a hook for cleaning")
  (extra     nil      :type plist       :doc "A Plist of extra data the backend can access")
  ;; TODO add a 'package load wrapper'. eg: for straight--load-package-autoloads
  )

(defun blood-build-backend (key &rest args) ;; -> nil|blood--backend-s
  "Builds and registers a backend"
  (log! :debug "Building Backend: %s" key)
  (cond ((gethash key blood--registered-backends)
         (gethash key blood--registered-backends)
         )
        (t
         (let ((backend (apply #'make-blood--backend-s :name key args)))
           (puthash key backend blood--registered-backends)
           backend
           )
         )
    )
  )

(defun blood--backend-activate-h ()
  "Activate default backend using blood--backend-default"
  (unless blood--backend-active
    (require (intern (format "blood--backend-%s" blood--backend-default)))
    (blood--set-backend blood--backend-default)
    )
  )

(defun blood--set-backend (key)
  (ilog! "Setting Backend to : %s" key)
 (setq blood--backend-active (gethash key blood--registered-backends))
  (unless blood--backend-active
    (error (format "Backend was not found: %s" key))
    )
  )

(provide 'blood-backend)
;;; blood-backend.el ends here
