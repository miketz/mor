;;; mode-on-region-tests.el --- Tests -*- lexical-binding: t -*-

;;; License: GPL version 3

;;; Commentary:
;;; Unit tests for package `mode-on-region'.
;;; Maybe other types of tests.
;;;
;;; Naming convention:
;;;  - prefix all test names with "mor-".
;;;  - include the word "test" in the name.
;;;
;;; Run tests:
;;;  - M-x ert RET "^mor-" RET

;;; Code:
(require 'mode-on-region)
(require 'ert)

(ert-deftest mor--overlap-p-test ()
  "Make sure region overlap detection is correct."
  (should-not (mor--overlap-p 1 10 15 20))
  (should-not (mor--overlap-p 2 10 -1 1))
  (should (mor--overlap-p 2 10 3 4))
  (should (mor--overlap-p 2 10 3 40))
  (should (mor--overlap-p 2 10 -3 5))
  (should (mor--overlap-p 5 20    ; big region
                          10 12)) ; little region completely inside big.
  (should (mor--overlap-p 10 12  ; little region
                          5 20)) ; big region completely covering little.
  ;; regions touching, but not overlapping
  (should-not (mor--overlap-p 1 2
                              3 4))
  (should-not (mor--overlap-p 3 4
                              1 2))
  ;; overlapping by 1
  (should (mor--overlap-p 1 2
                          2 3)))

(provide 'mode-on-region-tests)

;;; mode-on-region-tests.el ends here
