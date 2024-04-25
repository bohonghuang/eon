(in-package #:eon.editor)

(defmacro non-empty-or (result &optional default (handle-result '#'identity))
  (with-gensyms (var)
    `(let ((,var ,result))
       (if (emptyp ,var) ,default (funcall ,handle-result ,var)))))

(defmacro with-specified-value ((var val &optional default) &body body)
  `(let ((,var ,val)) (if (eql ,var :unspecified) ,default (progn . ,body))))

(defmacro specified-value (value specifiedp)
  `(if ,specifiedp ,value :unspecified))

(defun edit-integer (value &optional (min 0) (max most-positive-fixnum))
  (with-received-preview-function
    (let ((step 1))
      (async
        (do-non-nil (key (with-popped-prompt (format nil "~D (±~D)" value step)
                           (await (promise-pressed-key))))
          (case key
            ((:left :x) (setf step (max (truncate step 10) 1)))
            ((:right :y) (setf step (min (* step 10) 100)))
            (:up (incf value step))
            (:down (decf value step))
            (:a (return value))
            (:b (return nil)))
          (minf value max)
          (maxf value min)
          (preview value))))))

(defun edit-float (value)
  (unless value (setf value 0.0))
  (with-received-preview-function
    (let ((step 1.0))
      (async
        (do-non-nil (key (with-popped-prompt (format nil "~,2F (±~,2F)" value step)
                           (await (promise-pressed-key))))
          (case key
            ((:left :x) (setf step (max (/ step 10.0) 0.01)))
            ((:right :y) (setf step (min (* step 10.0) 100.0)))
            (:up (incf value step))
            (:down (decf value step))
            (:a (return value))
            (:b (return nil)))
          (preview value))))))

(defun edit-vector2 (value &optional (names '(x y)))
  (with-received-preview-function
    (let ((vector2 (copy-list value)) (step 1.0))
      (async
        (do-non-nil (key (with-popped-prompt (format nil "~A: ~,2F ~A: ~,2F (±~,2F)"
                                                     (first names) (first vector2)
                                                     (second names) (second vector2) step)
                           (await (promise-pressed-key))))
          (case key
            (:left (decf (first vector2) step))
            (:right (incf (first vector2) step))
            (:up (decf (second vector2) step))
            (:down (incf (second vector2) step))
            (:y (setf step (min (* step 10.0) 100.0)))
            (:x (setf step (max (/ step 10.0) 0.01)))
            (:a (return vector2))
            (:b (return nil)))
          (preview vector2))))))

(defun edit-rectangle (value &optional (names '(x y width height)))
  (with-received-preview-function
    (let ((rectangle (copy-list value)) (step 1.0))
      (async
        (do-non-nil (key (with-popped-prompt (format nil "~A: ~,2F ~A: ~,2F ~A: ~,2F ~A: ~,2F (±~,2F)"
                                                     (first names) (first rectangle)
                                                     (second names) (second rectangle)
                                                     (third names) (third rectangle)
                                                     (fourth names) (fourth rectangle) step)
                           (await (promise-pressed-key))))
          (case key
            (:y (setf step (min (* step 10.0) 100.0)))
            (:x (setf step (max (/ step 10.0) 0.01)))
            (:a (return rectangle))
            (:b (return nil))
            (t (if (key-down-p :l3)
                   (case key
                     (:left (decf (third rectangle) step))
                     (:right (incf (third rectangle) step))
                     (:up (decf (fourth rectangle) step))
                     (:down (incf (fourth rectangle) step)))
                   (case key
                     (:left (decf (first rectangle) step))
                     (:right (incf (first rectangle) step))
                     (:up (decf (second rectangle) step))
                     (:down (incf (second rectangle) step))))))
          (preview rectangle))))))
