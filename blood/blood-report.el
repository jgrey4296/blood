;;; blood-report.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- header
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey@Johns-Mac-mini.local>
;; Created: September 08, 2023
;; Modified: September 08, 2023
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 28.2))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:
;;-- end header
(loaded? blood-defs blood-log blood-utils)
(llog! "Report Lib")
(blood-register-cache! :report #'(lambda (data) data))

(defun blood-report--profile ()

  )

(defun blood-report--load-order ()

  )

(defun blood-report--memory ()

  )

(defun blood-report--load-time ()

  )

(defun blood-report-h ()
  "Hook for printing a report on the current profile:
everything in its spec (name, source, default, disabled, backend, etc)
"
  (hlog! "TODO Report")

  )

(defun report! ()
  "interactive report on blood state"
  (interactive)
  (hlog! "TODO: Generate Report")
  ;; load order, memory usage, load time, etc

  )

(provide 'blood-report)
;;; blood-report.el ends here
