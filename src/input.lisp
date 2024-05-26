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

(defun controller-button-keyboard-key (controller-button)
  "Get the RAYLIB:KEYBOARD-KEY corresponding to CONTROLLER-BUTTON."
  (declare (type keyword controller-button))
  (or (assoc-value *keyboard-key-mappings* controller-button)
      (error "Unknown controller button: ~S" controller-button)))

(defun keyboard-key-controller-button (keyboard-key)
  "Get the controller button corresponding to KEYBOARD-KEY."
  (declare (type fixnum keyboard-key))
  (rassoc-value *keyboard-key-mappings* keyboard-key))

(defun controller-button-pressed-p (button)
  "Return whether BUTTON has just been pressed."
  (raylib:is-key-pressed (controller-button-keyboard-key button)))

(defun controller-button-down-p (button)
  "Return whether BUTTON is being pressed."
  (raylib:is-key-down (controller-button-keyboard-key button)))

(defun controller-button-released-p (button)
  "Return whether BUTTON has just been released."
  (raylib:is-key-released (controller-button-keyboard-key button)))

(defun controller-button-up-p (button)
  "Return whether BUTTON is not being pressed."
  (raylib:is-key-up (controller-button-keyboard-key button)))

(defvar *previous-input-query-function* nil)

(defconstant +controller-button-queue-size-limit+ 1)

(defvar *controller-button-queue* nil
  "A queue of pressed keys. When a key is appended, it is considered to be pressed and returned by PRESSED-CONTROLLER-BUTTON or PROMISE-PRESSED-CONTROLLER-BUTTON.")

(defparameter *controller-button-repeat-enabled-p* t)

(defun pressed-controller-button ()
  (let ((keycode (raylib:get-key-pressed)))
    (if-let ((button (keyboard-key-controller-button keycode)))
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
      (pop *controller-button-queue*))))

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
