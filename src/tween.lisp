(in-package #:eon)

(setf (assoc-value *game-special-bindings* 'ute:*tween-manager*) '(ute:make-tween-manager))

(defun promise-tween (tween &optional (manager ute:*tween-manager*))
  "Start TWEEN by MANAGER, return a PROMISE:PROMISE which will be fulfilled when TWEEN is finished."
  (promise:with-promise (succeed)
    (assert (not (ute:startedp tween)))
    (assert (not (ute:finishedp tween)))
    (assert (not (ute:killedp tween)))
    (setf (ute:callback tween) (conjoin #'succeed (ute:callback tween)))
    (prog1 (ute:start tween manager)
      (ute::base-tween-update tween 0.0))))

(defmacro tween-iteration-in-place ((place sequence) &rest args &key (restore-place-p nil) &allow-other-keys)
  (remove-from-plistf args :restore-place-p)
  (with-gensyms (frame-count frame-sequence frame-index index store)
    `(let* ((,frame-sequence ,sequence)
            (,frame-count (length ,frame-sequence))
            (,frame-index 0))
       (flet ((,frame-index ()
                (integer-float ,frame-index))
              ((setf ,frame-index) (,index)
                (setf (integer-float ,frame-index) ,index)
                (when (< ,frame-index ,frame-count)
                  (setf ,place (elt ,frame-sequence ,frame-index)))))
         (ute:tween :to (((,frame-index)) ((integer-float ,frame-count)))
                    :callback (let ((,store ,place))
                                (lambda ()
                                  (when ,restore-place-p
                                    (setf ,place ,store))))
                    . ,args)))))
