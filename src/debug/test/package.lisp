(defpackage eon.debug.test
  (:use #:cl #:parachute)
  (:export #:suite))

(in-package #:eon.debug.test)

(define-test suite
  :parent (#:eon.test #:suite))
