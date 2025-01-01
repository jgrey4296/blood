;; blood--test.el -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; General ERT tests for blood
;;
(require 'buttercup)

(describe "deferally setup"
  (before-all
    (require 'blood-deferral))
  (it "is a sanity test" (expect t :to-be (not nil)))
  (it "should have defined vars"
    (expect (boundp 'blood-defer--load-queue) :to-be t)
    (expect (boundp 'blood-defer--load-timer) :to-be t)
    (expect (boundp 'blood-defer--skip-loads) :to-be t)
    )
  (it "should have defined functions"
    (expect (should (fboundp 'blood-defer--start-h)) :to-be t)
    (expect (should (fboundp 'file!)) :to-be t)
    (expect (should (fboundp 'dir!)) :to-be t)
    (expect (should (fboundp 'local-load!)) :to-be t)
    (expect (should (fboundp 'defer!)) :to-be t)
    (expect (should (fboundp 'defer-load!)) :to-be t)
    (expect (should (fboundp 'defer-require!)) :to-be t)
    )

  )

(describe "local load macro"
  (it "should expand paths properly"
    (expect (macroexpand '(local-load! "test"))
            :to-equal '(load (file-name-concat (dir!) "test") nil 'nomessage)
            )
    )
  (it "should skip when appropriate"
    (let ((blood-defer--skip-loads t))
      (expect (macroexpand '(local-load! "test")) :to-be nil)))

  )

(describe "defer macro"
  (it "should expand properly"
  (expecy (macroexpand '(defer! 10 "test"))
          :to-equal '(run-with-idle-timer 10 nil (lambda nil "test")))
    )
  )
