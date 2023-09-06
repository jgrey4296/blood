;;; macros.el -*- lexical-binding: t; -*-
;; A Simple load queue

(defvar defer--load-queue nil)

(defvar defer--load-timer nil)

(defvar defer--skip-loads nil)

(defconst defer--load-trys 5)

(defmacro file! ()
  "Return the file of the file this macro was called."
  (or (macroexp-file-name) load-file-name buffer-file-name)
)

(defmacro dir! ()
  "Return the directory of the file this macro was called.
from doom."
  (file-name-directory (file!)))

(defun defer--eval (wait-for instruction)
  (if wait-for
      (with-eval-after-load (car wait-for)
        (defer--eval (cdr wait-for) instruction))
    (pcase instruction
      (`(require ,x)
       (require x))
      (`(load ,x)
       (load x))
      (x
       (message "Unknown deferred eval instruction: %s" x)))))

(defun defer--queue-pop ()
  (when t ;; defer--load-timer
    (message "Deferred Loading, %s remain" (length defer--load-queue))
    (pcase (pop defer--load-queue)
      ((and `(after ,x ,y try ,z) (guard (consp x)) (guard (--every? (-contains? features it) x)))
       (add-to-list 'defer--load-queue y)
       (message "after list %s : %s" x y)
       )
      ((and `(after ,x ,y try ,z) (guard (zerop z)))
       ;; (with-eval-after-load (car x)
       ;;   (defer--eval (cdr x) y))
       (message "Incomplete after, no more tries, adding to after-load-functions : %s " y)
       )
      (`(after ,x ,y try ,z)
       (setq defer--load-queue (reverse (cons `(after ,x ,y try ,(1- z)) (reverse defer--load-queue))))
       (message "incomplete after %s : %s : remaining: %s" x y (1- z))
       )
      (`(require ,x)
       ;; (require x)
       (message "Requiring %s" x))
      (`(load ,x)
       ;; (load x)
       (message "Loading %s" x))
      (x (message "Unknown: %s" x)
         x
         )
      )
    )
  (when (and defer--load-timer (not defer--load-queue))
    (message "Defer Queue Empty, cancelling timer")
    (cancel-timer defer--load-timer))
)

(defun start-deferrals ()
  (interactive)
  (message "-------------------- Blood: Starting Deferrals")
  (when defer--load-timer
    (error 'timer-already-exists))
  (setq defer--load-timer (run-with-idle-timer 4 0.25 #'defer--queue-pop))
  )

(defmacro local-load! (filename &optional noerror)
  (unless defer--skip-loads
    `(load (file-name-concat (dir!) ,filename) ,noerror 'nomessage)
    )
  )

(defmacro defer! (time &rest body)
  `(run-with-idle-timer ,time nil
    (lambda ()
      ,@body
      )
    )
  )

(defmacro defer-load! (&optional afters &rest files)
  "Add the specified loads to the load queue"
  (let ((pathsym (gensym))
        (dirsym (gensym))
        )
    `(let* ((,dirsym (dir!))
            (,pathsym (mapcar (-partial #'file-name-concat ,dirsym) (if (stringp ,afters) (list ,afters ,@files) (list ,@files))))
            )
       (cond ((consp ,afters)
              (mapcar #'(lambda (x) (add-to-list 'defer--load-queue (list 'after ,afters (list 'load x) 'try defer--load-trys))) ,pathsym))
             (t
              (mapcar #'(lambda (x) (add-to-list 'defer--load-queue (list 'load x))) ,pathsym))
             )
       defer--load-queue
       )
    )
  )

(defmacro defer-require! (&optional afters &rest features)
  `(progn (cond ((consp ,afters)
                 (mapcar #'(lambda (x) (add-to-list 'defer--load-queue (list 'after ,afters (list 'require x) 'try defer--load-trys))) (list ,@features)))
                (t
                 (add-to-list 'defer--load-queue (list 'require ,afters))
                 (mapcar #'(lambda (x) (add-to-list 'defer--load-queue (list 'require x))) (list ,@features)))
                )

          defer--load-queue
          )
  )

(provide 'blood-deferral)
