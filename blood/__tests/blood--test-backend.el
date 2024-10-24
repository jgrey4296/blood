;;; blood--test-backend.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header


(require 'ert)
(require 'f)

(defun blood--test-backend--setup ()
  (add-to-list 'load-path (f-parent default-directory))
  (load "blood-backend")
)

(ert-deftest blood--test-backend-sanity-test ()
  "Initial Sanity Test"
  :tags '(sanity)
  (blood--test-backend--setup)
  (should (equal (pp-to-string '(quote quote)) "'quote"))
  (should (equal (pp-to-string '((quote a) (quote b))) "('a 'b)\n"))
  (should (equal (pp-to-string '('a 'b)) "('a 'b)\n"))
  )
