;; test.el -*- lexical-binding: t; -*-


(defun list-load-history ()
  (interactive)
  (insert "\nLoad History:\n<---- Oldest")
  (dolist-with-progress-reporter (top (reverse load-history))
      "Printing Load History"
    (let ((provides (--filter (eq (car-safe it) 'provide) (cdr top)))
          (requires (--filter (eq (car-safe it) 'require) (cdr top)))
          (file (car top))
          )
      (insert (format "\n%-30s reqs: %15s file: %s"
                      (string-join (mapcar #'(lambda (x) (symbol-name (cdr x))) provides) " ")
                      (string-join (mapcar #'(lambda (x) (symbol-name (cdr x))) requires) " ")
                      file))
      )
    )
  (insert "\n<---- Newest")
  )
