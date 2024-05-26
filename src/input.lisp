(in-package #:eon)

(define-constant +controller-buttons+
    '(:a :b :x :y :start :select :l1 :r1 :l2 :r2 :l3 :r3 :up :down :left :right)
  :test #'equal)

(defparameter *keyboard-key-mappings*
  (mapcar (lambda (pair)
            (cons (car pair) (foreign-enum-value 'raylib:keyboard-key (cdr pair))))
          '((:a . :x)
            (:b . :z)
            (:x . :s)
            (:y . :a)
            (:start . :enter)
            (:select . :backspace)
            (:l1 . :q)
            (:r1 . :w)
            (:l2 . :tab)
            (:r2 . :e)
            (:l3 . :left-control)
            (:r3 . :space)
            (:up . :up)
            (:down . :down)
            (:left . :left)
            (:right . :right)))
  "An association list used to represent the mapping of controller buttons to RAYLIB:KEYBOARD-KEYs.")

(defparameter *gamepad-button-mappings*
  (mapcar (lambda (pair)
            (cons (car pair) (foreign-enum-value 'raylib:gamepad-button (cdr pair))))
          '((:a . :right-face-right)
            (:b . :right-face-down)
            (:x . :right-face-up)
            (:y . :right-face-left)
            (:start . :middle-right)
            (:select . :middle-left)
            (:l1 . :left-trigger-1)
            (:r1 . :right-trigger-1)
            (:l2 . :left-trigger-2)
            (:r2 . :right-trigger-2)
            (:l3 . :left-thumb)
            (:r3 . :right-thumb)
            (:up . :left-face-up)
            (:down . :left-face-down)
            (:left . :left-face-left)
            (:right . :left-face-right)))
  "An association list used to represent the mapping of controller buttons to RAYLIB:GAMEPAD-BUTTONs.")

(defun controller-button-keyboard-key (controller-button)
  "Get the RAYLIB:KEYBOARD-KEY corresponding to CONTROLLER-BUTTON."
  (declare (type keyword controller-button))
  (or (assoc-value *keyboard-key-mappings* controller-button)
      (error "Unknown controller button: ~S" controller-button)))

(defun keyboard-key-controller-button (keyboard-key)
  "Get the controller button corresponding to KEYBOARD-KEY."
  (declare (type fixnum keyboard-key))
  (rassoc-value *keyboard-key-mappings* keyboard-key))

(defun controller-button-gamepad-button (controller-button)
  "Get the gamepad button corresponding to CONTROLLER-BUTTON."
  (or (assoc-value *gamepad-button-mappings* controller-button)
      (error "Unknown controller button: ~S" controller-button)))

(defun gamepad-button-controller-button (gamepad-button)
  "Get the controller button corresponding to GAMEPAD-BUTTON."
  (declare (type fixnum gamepad-button))
  (rassoc-value *gamepad-button-mappings* gamepad-button))

(defmacro do-available-gamepads ((var &optional (do :do)) &body body)
  `(loop :for ,var :of-type non-negative-fixnum :from 0
         :while (raylib:is-gamepad-available ,var)
         ,do (progn . ,body)))

(defun controller-button-pressed-p (button)
  "Return whether BUTTON has just been pressed."
  (or (raylib:is-key-pressed (controller-button-keyboard-key button))
      (let ((gamepad-button (controller-button-gamepad-button button)))
        (do-available-gamepads (gamepad :thereis)
          (raylib:is-gamepad-button-pressed gamepad gamepad-button)))))

(defun controller-button-down-p (button)
  "Return whether BUTTON is being pressed."
  (or (raylib:is-key-down (controller-button-keyboard-key button))
      (let ((gamepad-button (controller-button-gamepad-button button)))
        (do-available-gamepads (gamepad :thereis)
          (raylib:is-gamepad-button-down gamepad gamepad-button)))))

(defun controller-button-released-p (button)
  "Return whether BUTTON has just been released."
  (or (raylib:is-key-released (controller-button-keyboard-key button))
      (let ((gamepad-button (controller-button-gamepad-button button)))
        (do-available-gamepads (gamepad :thereis)
          (raylib:is-gamepad-button-released gamepad gamepad-button)))))

(defun controller-button-up-p (button)
  "Return whether BUTTON is not being pressed."
  (and (raylib:is-key-up (controller-button-keyboard-key button))
       (let ((gamepad-button (controller-button-gamepad-button button)))
         (do-available-gamepads (gamepad :always)
           (raylib:is-gamepad-button-up gamepad gamepad-button)))))

(defvar *previous-input-query-function* nil)

(defconstant +controller-button-queue-size-limit+ 1)

(defvar *controller-button-queue* nil
  "A queue of pressed keys. When a key is appended, it is considered to be pressed and returned by PRESSED-CONTROLLER-BUTTON or PROMISE-PRESSED-CONTROLLER-BUTTON.")

(defparameter *controller-button-repeat-enabled-p* t)

(defun pressed-controller-button ()
  (if-let ((button (or (when-let ((keycode (raylib:get-key-pressed)))
                         (keyboard-key-controller-button keycode))
                       (when-let ((button (raylib:get-gamepad-button-pressed)))
                         (gamepad-button-controller-button button)))))
    (let ((repeat-time (+ (game-loop-time) 0.5d0)))
      (when *controller-button-repeat-enabled-p*
        (add-game-loop-hook
         (lambda ()
           (if (> (game-loop-time) repeat-time)
               (async
                 (loop :while (and *controller-button-repeat-enabled-p* (controller-button-down-p button))
                       :when (< (length *controller-button-queue*) +controller-button-queue-size-limit+)
                         :do (nconcf *controller-button-queue* (list button))
                       :do (await (promise-sleep (/ 5.0 60.0)))
                       :finally (setf *controller-button-queue* nil)))
               (controller-button-up-p button)))
         :before #'not))
      (setf *previous-input-query-function* #'pressed-controller-button)
      (return-from pressed-controller-button button))
    (pop *controller-button-queue*)))

(defun pressed-char ()
  "Get the recently pressed character, returning NIL if none was pressed."
  (let ((code (raylib:get-char-pressed))
        (key (raylib:get-key-pressed)))
    (if (zerop code)
        (case key
          (#.(foreign-enum-value 'raylib:keyboard-key :enter) #\Return)
          (#.(foreign-enum-value 'raylib:keyboard-key :backspace) #\Rubout)
          (t (when (or (raylib:is-key-down #.(foreign-enum-value 'raylib:keyboard-key :left-control))
                       (raylib:is-key-down #.(foreign-enum-value 'raylib:keyboard-key :right-control)))
               (case key
                 (#.(foreign-enum-value 'raylib:keyboard-key :z) #\Sub)
                 (#.(foreign-enum-value 'raylib:keyboard-key :x) #\Can)
                 (#.(foreign-enum-value 'raylib:keyboard-key :c) #\Etx)
                 (#.(foreign-enum-value 'raylib:keyboard-key :v) #\Syn)))))
        (if (eql *previous-input-query-function* #'pressed-char)
            (code-char code)
            (progn
              (setf *previous-input-query-function* #'pressed-char)
              (return-from pressed-char nil))))))

(defun promise-pressed-char ()
  "Wait for a character to be pressed and return a PROMISE:PROMISE of it."
  (promise:with-promise (succeed)
    (add-game-loop-hook
     (lambda ()
       (when-let ((char (pressed-char)))
         (succeed char)))
     :before #'not)))

(defun promise-pressed-controller-button ()
  "Wait for a key to be pressed and return a PROMISE:PROMISE of it."
  (promise:with-promise (succeed)
    (add-game-loop-hook
     (lambda ()
       (when-let ((key (pressed-controller-button)))
         (succeed key)))
     :before #'not)))
