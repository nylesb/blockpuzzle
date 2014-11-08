;;;; Contains unit tests for the blockpuzzle program, main.lisp.
;;;; This file runs all tests and displays results

;;; Preparation

(load "main.lisp")

;;; Runs a function while supressing standard output printing
(defun funcall-suppressed (function)
  (with-open-stream (*standard-output* (make-broadcast-stream))
    (funcall function)))

;;; Catches errors testcase might throw, then prints test results to output.
(defun run-test (testcase) ; testcase a function
  (let ((error-capture (make-string-output-stream)))
    (if (with-open-stream (*error-output* (make-broadcast-stream error-capture))
          (ignore-errors (funcall-suppressed testcase)))
        (princ (format nil "  O Pass: ~A ~%" (documentation testcase 'function)))
        (princ (format nil " X  Fail: ~A ~%" (documentation testcase 'function))))
    (princ (get-output-stream-string error-capture))))

;;; Throws an error if unequal, allowing run-test to catch test failures.
;;; Also sends an error message to show expected vs actual result.
(defun assert-equal (expected actual)
  (if (equal expected actual)
      (eval t)
      (progn (format *error-output* "      Expected: ~A~%" expected)
             (format *error-output* "      Actual  : ~A.~%" actual)
             (error ""))))

;;; Test cases
;;; Tests formatted by passing the test function into run-test.
;;; The test's doc string will display as the test description.
;;; Use assert-equal to signal test success or failure.

(princ (format nil "~%~%-> Tests for functionality of blockpuzzle~%~%"))

(run-test (lambda ()
  "Should check for valid starting positions & display success on exit."
  (let ((myvar "Cat"))
    (assert-equal myvar "Caper"))))