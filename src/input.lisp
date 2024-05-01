(in-package #:eon)

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
  "An association list used to represent the mapping of gamepad buttons to keyboard keys.")

(defun key-keyboard (key)
  "Get the keyboard key corresponding to KEY."
  (declare (type keyword key))
  (or (assoc-value *keyboard-key-mappings* key)
      (error "Unknown input key: ~S" key)))

(defun keyboard-key (key)
  "Get the key corresponding to keyboard KEY."
  (declare (type fixnum key))
  (rassoc-value *keyboard-key-mappings* key))

(defun key-pressed-p (key)
  "Return whether KEY has just been pressed."
  (raylib:is-key-pressed (key-keyboard key)))

(defun key-down-p (key)
  "Return whether KEY is being pressed."
  (raylib:is-key-down (key-keyboard key)))

(defun key-released-p (key)
  "Return whether KEY has just been released."
  (raylib:is-key-released (key-keyboard key)))

(defun key-up-p (key)
  "Return whether KEY is not being pressed."
  (raylib:is-key-up (key-keyboard key)))

(defvar *previous-input-query-function* nil)

(defconstant +key-queue-size-limit+ 1)

(defvar *key-queue* nil
  "A queue of pressed keys. When a key is appended, it is considered to be pressed and returned by PRESSED-KEY or PROMISE-PRESSED-KEY.")

(defparameter *key-repeat-enabled-p* t)

(defun pressed-key ()
  (let ((keycode (raylib:get-key-pressed)))
    (if-let ((key (keyboard-key keycode)))
      (let ((repeat-time (+ (game-loop-time) 0.5d0)))
        (when *key-repeat-enabled-p*
          (add-game-loop-hook
           (lambda ()
             (if (> (game-loop-time) repeat-time)
                 (async
                   (loop :while (and *key-repeat-enabled-p* (key-down-p key))
                         :when (< (length *key-queue*) +key-queue-size-limit+)
                           :do (nconcf *key-queue* (list key))
                         :do (await (promise-sleep (/ 5.0 60.0)))
                         :finally (setf *key-queue* nil)))
                 (key-up-p key)))
           :before #'not))
        (setf *previous-input-query-function* #'pressed-key)
        (return-from pressed-key key))
      (pop *key-queue*))))

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

(defun promise-pressed-key ()
  "Wait for a key to be pressed and return a PROMISE:PROMISE of it."
  (promise:with-promise (succeed)
    (add-game-loop-hook
     (lambda ()
       (when-let ((key (pressed-key)))
         (succeed key)))
     :before #'not)))
