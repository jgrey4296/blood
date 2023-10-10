;;; macros.el -*- lexical-binding: t; -*-
;; A Simple load queue
(llog! "Deferrals")

(defvar blood-defer--load-queue nil)

(defvar blood-defer--load-timer nil)

(defvar blood-defer--skip-loads nil)

(defconst blood-defer--load-trys 5)

(defun blood-defer--start-h ()
  (interactive)
  (hlog! "Starting Deferrals")
  (when blood-defer--load-timer
    (error 'timer-already-exists))
  (setq blood-defer--load-timer (run-with-idle-timer 4 0.25 #'blood-defer--queue-pop))
  )

(defmacro file! ()
  "Return the file of the file this macro was called."
  (or (macroexp-file-name) load-file-name buffer-file-name)
)

(defmacro dir! ()
  "Return the directory of the file this macro was called.
from doom."
  `(file-name-directory (file!)))

(defun blood-defer--eval (wait-for instruction)
  (if wait-for
      (with-eval-after-load (car wait-for)
        (blood-defer--eval (cdr wait-for) instruction))
    (pcase instruction
      (`(require ,x)
       (require x))
      (`(load ,x)
       (load x))
      (x
       (ilog! "Unknown deferred eval instruction: %s" x)))))

(defun blood-defer--queue-pop ()
  (when t ;; blood-defer--load-timer
    (ilog! "Deferred Loading, %s remain" (length blood-defer--load-queue))
    (pcase (pop blood-defer--load-queue)
      ((and `(after ,x ,y try ,z) (guard (consp x)) (guard (--every? (-contains? features it) x)))
       (add-to-list 'blood-defer--load-queue y)
       (ilog! "after list %s : %s" x y)
       )
      ((and `(after ,x ,y try ,z) (guard (zerop z)))
       ;; (with-eval-after-load (car x)
       ;;   (blood-defer--eval (cdr x) y))
       (ilog! "Incomplete after, no more tries, adding to after-load-functions : %s " y)
       )
      (`(after ,x ,y try ,z)
       (setq blood-defer--load-queue (reverse (cons `(after ,x ,y try ,(1- z)) (reverse blood-defer--load-queue))))
       (ilog! "incomplete after %s : %s : remaining: %s" x y (1- z))
       )
      (`(require ,x)
       (llog! "%s (require)" x)
       (require x)
       )
      (`(load ,x)
       (llog! "%s (load)" x)
       (load x nil t)
       )
      (x (ilog! "Unknown: %s" x)
         x
         )
      )
    )
  (when (and blood-defer--load-timer (not blood-defer--load-queue))
    (hlog! "Defer Queue Empty, cancelling timer")
    (cancel-timer blood-defer--load-timer))
)

(defmacro local-load! (filename &optional noerror)
  (unless blood-defer--skip-loads
    `(load (file-name-concat (dir!) ,filename) ,noerror 'nomessage)
    )
  )

(defmacro defer! (time &rest body)
  `(unless blood-defer--skip-loads
     (run-with-idle-timer ,time nil
                          (lambda ()
                            ,@body
                            )
                          )
     )
  )

(defmacro defer-load! (&optional afters &rest files)
  "Add the specified loads to the load queue"
    (let ((pathsym (gensym))
          (dirsym (gensym))
          )
      `(unless blood-defer--skip-loads
         (let* ((,dirsym (dir!))
                (,pathsym (mapcar (lambda (x) (file-name-concat ,dirsym x)) (if (stringp ,afters) (list ,afters ,@files) (list ,@files))))
                )
           (cond ((consp ,afters)
                  (mapcar #'(lambda (x) (add-to-list 'blood-defer--load-queue (list 'after ,afters (list 'load x) 'try blood-defer--load-trys))) ,pathsym))
                 (t
                  (mapcar #'(lambda (x) (add-to-list 'blood-defer--load-queue (list 'load x))) ,pathsym))
                 )
           blood-defer--load-queue
           )
         )
      )
    )

(defmacro defer-require! (&optional afters &rest features)
  `(unless blood-defer--skip-loads
     (cond ((consp ,afters)
            (mapcar #'(lambda (x) (add-to-list 'blood-defer--load-queue (list 'after ,afters (list 'require x) 'try blood-defer--load-trys))) (list ,@features)))
           (t
            (add-to-list 'blood-defer--load-queue (list 'require ,afters))
            (mapcar #'(lambda (x) (add-to-list 'blood-defer--load-queue (list 'require x))) (list ,@features)))
           )

     blood-defer--load-queue
     )
  )

(provide 'blood-deferral)
