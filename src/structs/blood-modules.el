;;; blood-modules.el -*- lexical-binding: t; no-byte-compile: t; -*-
(loaded? blood-structs)
(llog! "Modules lib")
(require 'blood-sync)
(require 'blood-trace)

(defconst BLOOD-MODULE-FILE-PATTERN--FD ".*(m-.+|module|module-.+).el$"  "blood will search and load all files named this in module directories, to get package specs")

(defvar blood-modules--declared-components-ht (make-hash-table) "Maps group:module:package to package spec. sym -> component. `use!` adds to this.")

(defvar blood-modules--package-component-map-ht (make-hash-table) "Maps packages to all modules they are part of. sym -> list[sym]")

(defun blood-modules--sym-from-parts (group mod &optional package)
  "Build a symbol from a group, a module name, and maybe a package to uniquely
identify a module component
TODO: refactor into blood-structs
"
  (intern (if package
              (format "%s:%s:%s" group mod package)
            (format "%s:%s" group mod))))

(defun blood-modules--init-packages-h ()
  "Start the active profile's modules"
  (blood--expand-loadpath)
  (hlog! "Initialising Packages")
  (let* ((profile (blood-profile-current))
         (mod-files (blood-sync--module-specs-of-profile profile)) ;; get active modules
         (components  (blood-sync--collect-module-component-specs mod-files 'allow))
         )
    (ilog! "Found %s components to initialise" (length components))
    (dolist (package-spec components)
      (blood-packages--init package-spec)
      )
    )
  )

(defun blood-modules--config-packages-h ()
  "Config the active profile's modules"
  (hlog! "Configuring and Loading Components")
  (blood-read-cache! :dag)
  (let* ((components (apply #'append (mapcar #'(lambda (x) (gethash x blood-modules--package-component-map-ht)) blood-dag--order)))
         (specs      (apply #'append (mapcar #'(lambda (y) (list (gethash y blood-modules--declared-components-ht))) components)))
        )
    (ghlog! "Components to load: %s" (length specs))
    ;; Load the dag calculated order
    (dolist (comp-spec specs)
      (blood-packages--config comp-spec)
      )
    )
  (glogxs!)
  )

(provide 'blood-modules)
;;; blood-modules.el ends here
