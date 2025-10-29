;;; more-buttercup-matchers.el -*- lexical-binding: t; no-byte-compile: t; -*-

(buttercup-define-matcher :to-signal (expr &optional signal signal-args)
  "Defines :to-signal, which correctly catches cl-assert failures"
  (let ((expected-signal-symbol (and signal (funcall signal)))
        (expected-signal-args (and signal-args (funcall signal-args)))
        (unevaluated-expr (buttercup--enclosed-expr expr))
        (debug-on-error nil)
        expr-value
        thrown-signal)
    ;; Set the above variables
    (condition-case err
        (setq expr-value (funcall expr))
      (cl--assertion-failed
       (setq thrown-signal err)
       nil)
      (error
       (setq thrown-signal err)
       nil)
      )
    (buttercup--handle-to-throw thrown-signal
                                (cons expected-signal-symbol expected-signal-args)
                                unevaluated-expr expr-value)))

(buttercup-define-matcher :to-pass (expr test)
  "Check the expr's return value passes a bool test.
eg: cl-{class}-p "
  (let ((actual-test (and test (funcall test))))
    (if (funcall actual-test (funcall expr))
        (cons t (format "Didn't expect to pass %s, but did" actual-test))
      (cons nil (format "Expected to pass %s, but didn't" actual-test))
        )
    )
  )

(buttercup-define-matcher :slot (expr type slot matcher &rest args)
  "Get the slot from a struct slot of the result of expr "
  (let* ((expr-result (funcall expr))
         (uneval-expr (buttercup--enclosed-expr expr))
         (slot-val (cl-struct-slot-value (funcall type)
                                         (funcall slot)
                                         expr-result))
         (slotw `(lambda () (quote ,uneval-expr) (quote ,slot-val)))
         (matcher (funcall matcher))
         )
    (buttercup--apply-matcher matcher (cons slotw args))
    )
)

(buttercup-define-matcher :has-length (expr val)
  (let ((ex-val (funcall expr)))
    (and (sequencep ex-val)
         (eq (seq-length ex-val)
             (funcall val)))
    )
  )


(provide 'extra-buttercup-matchers)
;; Local Variables:
;; read-symbol-shorthands: (
;; ("mbc-" . "more-buttercup-matchers-")
;; )
;; End:
;;; more-buttercup-matchers.el ends here
