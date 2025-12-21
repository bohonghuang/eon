(in-package #:eon)

(defstruct (virtual-keyboard-border-entry (:include select-box-border-entry))
  (cell (make-scene2d-max-cell :size (raylib:make-vector2 :x 15.0 :y 22.0)) :type scene2d-max-cell)
  (size (raylib:vector2-zero) :type raylib:vector2))

(defun virtual-keyboard-border-entry (content)
  (let ((entry (make-virtual-keyboard-border-entry :content content)))
    (setf (scene2d-max-cell-content (virtual-keyboard-border-entry-cell entry)) content)
    entry))

(defmethod scene2d-bound ((entry virtual-keyboard-border-entry))
  (size-rectangle (virtual-keyboard-border-entry-size entry)))

(defmethod scene2d-layout ((entry virtual-keyboard-border-entry))
  (call-next-method)
  (scene2d-layout (virtual-keyboard-border-entry-cell entry))
  (raylib:copy-vector2
   (scene2d-size (virtual-keyboard-border-entry-cell entry))
   (virtual-keyboard-border-entry-size entry)))

(defstruct virtual-keyboard-key-style
  "A structure describing the key style in VIRTUAL-KEYBOARD."
  (label-style (make-scene2d-label-style) :type scene2d-label-style)
  (entry-type 'virtual-keyboard-border-entry :type (or symbol function)))

(define-scene2d-default-construct-form virtual-keyboard-key-style (label-style entry-type))

(defstruct virtual-keyboard-style
  "A structure describing the style of VIRTUAL-KEYBOARD."
  (letter-key-style (make-virtual-keyboard-key-style) :type virtual-keyboard-key-style)
  (enter-key-style (make-virtual-keyboard-key-style) :type virtual-keyboard-key-style)
  (shift-key-style (make-virtual-keyboard-key-style) :type virtual-keyboard-key-style)
  (backspace-key-style (make-virtual-keyboard-key-style) :type virtual-keyboard-key-style)
  (space-key-style (make-virtual-keyboard-key-style) :type virtual-keyboard-key-style)
  (row-spacing 4.0 :type single-float))

(define-scene2d-default-construct-form virtual-keyboard-style
    (letter-key-style enter-key-style shift-key-style backspace-key-style space-key-style row-spacing))

(defstruct (virtual-keyboard (:include scene2d-container)
                             (:constructor %make-virtual-keyboard))
  "A SCENE2D-NODE that allows the user to select characters on the keyboard using controller buttons."
  (style (make-virtual-keyboard-style) :type virtual-keyboard-style))

(defun virtual-keyboard-ensure-string (object)
  (etypecase object
    (base-char (make-array 1 :element-type 'base-char :initial-element object))
    (simple-string (coerce object 'simple-base-string))))

(defun make-virtual-keyboard (&rest args &key (style (make-virtual-keyboard-style)) &allow-other-keys)
  (flet ((key (value &optional (style (virtual-keyboard-style-letter-key-style style)))
           (funcall
            (virtual-keyboard-key-style-entry-type style)
            (scene2d-construct
             (scene2d-margin
              :all 2.0
              :child (scene2d-label
                      :string (virtual-keyboard-ensure-string value)
                      :style (virtual-keyboard-key-style-label-style style)))))))
    (let ((row-spacing (virtual-keyboard-style-row-spacing style)))
      (apply
       #'%make-virtual-keyboard
       :content (scene2d-construct
                 (scene2d-box
                  :orientation :vertical
                  :children ((scene2d-box
                              :orientation :horizontal
                              :children (nconc
                                         (loop :for char :across "1234567890-=" :collect (key char))
                                         (list (key "DELETE" (virtual-keyboard-style-backspace-key-style style)))))
                             (scene2d-box :children ((scene2d-margin :top row-spacing)))
                             (scene2d-box
                              :orientation :horizontal
                              :children (nconc
                                         (loop :for char :across "qwertyuiop[]\\" :collect (key char))))
                             (scene2d-box :children ((scene2d-margin :top row-spacing)))
                             (scene2d-box
                              :orientation :horizontal
                              :children (nconc
                                         (loop :for char :across "asdfghjkl;'" :collect (key char))
                                         (list (key "ENTER" (virtual-keyboard-style-backspace-key-style style)))))
                             (scene2d-box :children ((scene2d-margin :top row-spacing)))
                             (scene2d-box
                              :orientation :horizontal
                              :children (nconc
                                         (list (key "SHIFT" (virtual-keyboard-style-backspace-key-style style)))
                                         (loop :for char :across "zxcvbnm,./" :collect (key char))
                                         (list (key "SHIFT" (virtual-keyboard-style-backspace-key-style style)))))
                             (scene2d-box :children ((scene2d-margin :top row-spacing)))
                             (scene2d-box
                              :orientation :horizontal
                              :children ((key "SPACE" (virtual-keyboard-style-space-key-style style)))))))
       args))))

(define-scene2d-default-construct-form virtual-keyboard (style))

(defun virtual-keyboard-key-label (key)
  (scene2d-margin-content (scene2d-focusable-content key)))

(defun virtual-keyboard-key-string (key)
  (scene2d-label-string (virtual-keyboard-key-label key)))

(defun virtual-keyboard-keys (keyboard &optional row)
  (delete-if-not
   #'scene2d-focusable-p
   (if row
       (scene2d-box-children (nth row (scene2d-box-children (virtual-keyboard-content keyboard))))
       (mapcan #'scene2d-box-children (scene2d-box-children (virtual-keyboard-content keyboard))))))

(defmethod selectable-container-entries ((keyboard virtual-keyboard))
  (virtual-keyboard-keys keyboard))

(define-constant +virtual-keyboard-key-case-alist+
    '((#\1 . #\!) (#\2 . #\@) (#\3 . #\#) (#\4 . #\$) (#\5 . #\%) (#\6 . #\^) (#\7 . #\&) (#\8 . #\*) (#\9 . #\() (#\0 . #\)) (#\- . #\_) (#\= . #\+)
      (#\q . #\Q) (#\w . #\W) (#\e . #\E) (#\r . #\R) (#\t . #\T) (#\y . #\Y) (#\u . #\U) (#\i . #\I) (#\o . #\O) (#\p . #\P) (#\[ . #\{) (#\] . #\}) (#\\ . #\|)
      (#\a . #\A) (#\s . #\S) (#\d . #\D) (#\f . #\F) (#\g . #\G) (#\h . #\H) (#\j . #\J) (#\k . #\K) (#\l . #\L) (#\; . #\:) (#\' . #\")
      (#\z . #\Z) (#\x . #\X) (#\c . #\C) (#\v . #\V) (#\b . #\B) (#\n . #\N) (#\m . #\M) (#\, . #\<) (#\. . #\>) (#\/ . #\?))
  :test #'equal)

(defun virtual-keyboard-upper-case-p (keyboard)
  (loop :for key :in (virtual-keyboard-keys keyboard)
        :for string := (virtual-keyboard-key-string key)
        :thereis (and (= (length string) 1) (upper-case-p (first-elt string)))))

(defun virtual-keyboard-toggle-case (keyboard)
  (loop :with assoc-value :of-type (function (list base-char) (values base-char t))
          := (if (virtual-keyboard-upper-case-p keyboard) #'rassoc-value #'assoc-value)
        :for key :in (virtual-keyboard-keys keyboard)
        :for label := (virtual-keyboard-key-label key)
        :for string :of-type simple-base-string := (scene2d-label-string label)
        :when (= (length string) 1)
          :do (setf (scene2d-label-string label) (virtual-keyboard-ensure-string (funcall assoc-value +virtual-keyboard-key-case-alist+ (first-elt string))))
        :finally
           (scene2d-layout keyboard)))

(defun virtual-keyboard-promise-char (keyboard)
  "Allow the user to start selecting a character on the KEYBOARD using controller buttons. When a character is selected, the returned PROMISE is fulfilled with that character."
  (async
    (loop
      (if-let ((index (await (selectable-container-promise-index keyboard nil))))
        (let ((string (virtual-keyboard-key-string (nth index (virtual-keyboard-keys keyboard)))))
          (if (= (length string) 1)
              (return (first-elt string))
              (eswitch (string :test #'string=)
                ("DELETE" (return #\Backspace))
                ("ENTER" (return #\Return))
                ("SPACE" (return #\Space))
                ("SHIFT" (virtual-keyboard-toggle-case keyboard)))))
        (return nil)))))
