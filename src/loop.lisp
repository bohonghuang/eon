(in-package #:eon)

(declaim (ftype (function () (values single-float)) game-loop-delta-time))
(defun game-loop-delta-time ()
  "Get the interval time in seconds between two game loop iterations."
  (cond
    ((raylib:is-key-down #.(foreign-enum-value 'raylib:keyboard-key :kp-0)) (* 4.0 (raylib:get-frame-time)))
    ((raylib:is-key-down #.(foreign-enum-value 'raylib:keyboard-key :kp-decimal)) (* 0.5 (raylib:get-frame-time)))
    (t (raylib:get-frame-time))))

(declaim (ftype (function () (values double-float)) game-loop-time)
         (inline game-loop-time))
(defun game-loop-time ()
  "Get elapsed time in seconds since the first loop iteration started."
  (raylib:get-time))

(defstruct game-loop-context
  (thread (bt2:current-thread) :type bt2:thread)
  (loop-begin-hook nil :type list)
  (loop-end-hook nil :type list)
  (hook-lock (bt2:make-lock) :type bt2:lock :read-only t))

(defvar *game-loop-context* nil)

(defun add-game-loop-hook (hook type repeat &aux (context *game-loop-context*))
  "Thread-safely add HOOK to the game loop before or after (determined by TYPE as :BEFORE or :AFTER), with the execution frequency of HOOK determined by REPEAT.  

REPEAT can be:
- A FUNCTION that filters the execution results of HOOK. When this function returns a non-NIL value, HOOK will continue to be executed in the next loop.
- A POSITIVE-FIXNUM indicating how many times HOOK will be executed.
- A BOOLEAN. If it is NIL, HOOK will be executed only once. Otherwise, it will be continuously executed until REMOVE-GAME-LOOP-HOOK is called on it."
  (macrolet ((add-hook (hook-var)
               `(let ((repeat-function (etypecase repeat
                                         (boolean (constantly repeat))
                                         (positive-fixnum (lambda (result)
                                                            (declare (ignore result))
                                                            (plusp (decf repeat))))
                                         (function (curry #'funcall repeat))))
                      (original-hook hook))
                  (setf hook (lambda () (funcall repeat-function (funcall original-hook))))
                  (bt2:with-lock-held ((game-loop-context-hook-lock context))
                    (push hook ,hook-var)))))
    (ecase type
      ((:begin :before)
       (add-hook (game-loop-context-loop-begin-hook context)))
      ((:end :after)
       (add-hook (game-loop-context-loop-end-hook context))))
    hook))

(defvar *game-loop-hook-deleted* (constantly nil))

(defun remove-game-loop-hook (hook &aux (context *game-loop-context*))
  "Thread-safely remove HOOK from the game loop."
  (bt2:with-lock-held ((game-loop-context-hook-lock context))
    (let ((hook-deleted *game-loop-hook-deleted*))
      (loop :for hook-cons :on (game-loop-context-loop-begin-hook context)
            :when (eq (car hook-cons) hook)
              :do (setf (car hook-cons) hook-deleted))
      (loop :for hook-cons :on (game-loop-context-loop-end-hook context)
            :when (eq (car hook-cons) hook)
              :do (setf (car hook-cons) hook-deleted)))))

(defmacro run-game-loop-hook (hook-var &optional (context '*game-loop-context*))
  (with-gensyms (hook-deleted hook-cons)
    `(loop :with ,hook-deleted := *game-loop-hook-deleted*
           :for ,hook-cons :on ,hook-var
           :unless (funcall (car ,hook-cons))
             :do (setf (car ,hook-cons) ,hook-deleted)
           :finally
              (bt2:with-lock-held ((game-loop-context-hook-lock ,context))
                (deletef ,hook-var ,hook-deleted :test #'eq)))))

(defmacro do-game-loop (&body body)
  "Run the game loop, executing BODY once per loop iteration."
  (with-gensyms (context delta)
    `(loop :with ,context := *game-loop-context*
           :until (raylib:window-should-close)
           :for ,delta :of-type single-float := (game-loop-delta-time)
           :do (progn
                 (run-game-loop-hook (game-loop-context-loop-begin-hook ,context))
                 ,@body
                 (ute:update ,delta)
                 (promise:tick-all ,delta)
                 (run-game-loop-hook (game-loop-context-loop-end-hook ,context))))))

(defun promise-sleep (time)
  "Non-blockingly sleep for the specified number of seconds indicated by TIME, and the returned PROMISE:PROMISE will be fulfilled afterwards."
  (let* ((sleep-secs (coerce time 'single-float))
         (secs 0.0))
    (promise:with-promise (succeed)
      (add-game-loop-hook
       (lambda ()
         (when (>= (incf secs (game-loop-delta-time)) sleep-secs)
           (succeed)))
       :before #'not))))

(defmacro with-lparallel-kernel (args &body body)
  `(let ((lparallel:*kernel* (lparallel:make-kernel . ,args)))
     (unwind-protect (progn . ,body)
       (lparallel:end-kernel))))

(defun promise-task (task)
  "Send TASK to be executed in another worker non-blockingly, and its execution result will be returned as a PROMISE:PROMISE."
  (promise:with-promise (succeed)
    (let ((game-loop-context *game-loop-context*))
      (lparallel:future
        (let ((*game-loop-context* game-loop-context))
          (let ((result (funcall task)))
            (add-game-loop-hook (curry #'succeed result) :after nil)))))))

(defparameter *game-special-bindings* (list '(*game-loop-context* . (make-game-loop-context))))

(defmacro with-game-context (&body body)
  "Execute BODY within the game context."
  `(with-asset-manager
     (raylib:with-audio-device
       (with-lparallel-kernel (4)
         (let* ,(mapcar (lambda (binding) (list (car binding) (cdr binding))) (reverse *game-special-bindings*))
           (unwind-protect (progn . ,body) (promise:clear)))))))

(defvar *game-loop-once-only-table* (make-hash-table :test #'eq))

(setf (assoc-value eon::*game-special-bindings* '*game-loop-once-only-table*)
      (with-gensyms (table sub-table)
        `(let ((,table (make-hash-table :test #'eq)))
           (add-game-loop-hook
            (lambda ()
              (loop :for ,sub-table :being :each hash-value :of ,table
                    :do (clrhash ,sub-table)))
            :before t)
           ,table)))

(defmacro game-loop-once-only (objects &body body)
  "Ensure that BODY is executed only once per game loop iteration for the same set of OBJECTS. If OBJECTS is NIL, BODY is executed only once per any game loop iteration, regardless of how many times the entire form is executed."
  (unless objects (setf objects `(',(gensym))))
  (with-gensyms (table identifier)
    `(let* ((,table *game-loop-once-only-table*)
            (,table (ensure-gethash ',identifier ,table (make-hash-table))))
       ,(loop :for body-form := `(progn . ,body)
                :then `(unless (nth-value 1 (ensure-gethash ,object ,table t))
                         ,body-form)
              :for object :in objects
              :finally (return body-form)))))
