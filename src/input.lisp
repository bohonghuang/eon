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
            (:right . :right))))

(defun key-keyboard (key)
  (declare (type keyword key))
  (or (assoc-value *keyboard-key-mappings* key)
      (error "Unknown input key: ~S" key)))

(defun keyboard-key (key)
  (declare (type fixnum key))
  (rassoc-value *keyboard-key-mappings* key))

(defun key-pressed-p (key)
  (raylib:is-key-pressed (key-keyboard key)))

(defun key-down-p (key)
  (raylib:is-key-down (key-keyboard key)))

(defun key-released-p (key)
  (raylib:is-key-released (key-keyboard key)))

(defun key-up-p (key)
  (raylib:is-key-up (key-keyboard key)))

(defvar *previous-input-query-function* nil)

(defconstant +key-queue-size-limit+ 1)

(defvar *key-queue* nil)

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
                           :do (push key *key-queue*)
                         :do (await (promise-sleep (/ 5.0 60.0)))
                         :finally (setf *key-queue* nil)))
                 (key-up-p key)))
           :before #'not))
        (setf *previous-input-query-function* #'pressed-key)
        (return-from pressed-key key))
      (pop *key-queue*))))

(defun pressed-char ()
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
  (promise:with-promise (succeed)
    (add-game-loop-hook
     (lambda ()
       (when-let ((char (pressed-char)))
         (succeed char)))
     :before #'not)))

(defun promise-pressed-key ()
  (promise:with-promise (succeed)
    (add-game-loop-hook
     (lambda ()
       (when-let ((key (pressed-key)))
         (succeed key)))
     :before #'not)))
