(in-package #:eon)

(declaim (ftype (function (single-float) single-float) radian-degree)
         (inline radian-degree))
(defun radian-degree (rad)
  (* rad (/ 360.0 (* 2.0 (coerce pi 'single-float)))))

(declaim (ftype (function (single-float) single-float) degree-radian)
         (inline degree-radian))
(defun degree-radian (deg)
  (* deg (/ (* 2.0 (coerce pi 'single-float)) 360.0)))

(declaim (inline integer-float))
(defun integer-float (integer)
  (coerce integer 'single-float))

(define-setf-expander integer-float (integer &environment env)
  (multiple-value-bind (vars vals newval setter getter) (get-setf-expansion integer env)
    (declare (ignore newval setter))
    (with-gensyms (store)
      (values vars vals `(,store) `(setf ,getter (values (truncate ,store))) `(integer-float ,getter)))))

(declaim (inline array-vector))
(defun array-vector (array)
  (loop :with size := (array-total-size array)
        :with vector := (make-array size :element-type (array-element-type array))
        :for i :below size
        :do (setf (aref vector i) (row-major-aref array i))
        :finally (return vector)))
