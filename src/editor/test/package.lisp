(defpackage eon.editor.test
  (:use #:cl #:parachute)
  (:export #:suite))

(in-package #:eon.editor.test)

(define-test suite
  :parent (#:eon.test #:suite))
