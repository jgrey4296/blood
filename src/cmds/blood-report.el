;;; blood-report.el -*- lexical-binding: t; no-byte-compile: t; -*-
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
