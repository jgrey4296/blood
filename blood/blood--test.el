;; blood--test.el -*- mode: elisp; lexical-binding: t; -*-
;;
(defun blood-test-h ()
  (ilog! "TODO"
  )


(ert-deftest blood-test-quote ()
  "Tests the rendering of `quote' symbols in `pp-to-string'."
  (should (equal (pp-to-string '(quote quote)) "'quote"))
  (should (equal (pp-to-string '((quote a) (quote b))) "('a 'b)\n"))
  (should (equal (pp-to-string '('a 'b)) "('a 'b)\n")))

(ert-deftest blood-test-math ()
  "Tests the rendering of `quote' symbols in `pp-to-string'."
  (should (equal 2 (* 2 1)))
  (should (equal 3 (- 5 2)))
  (should (equal 4 (+ 2 3)))
  )
