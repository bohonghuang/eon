(in-package #:eon)

(defstruct dialog-box-text-style
  "A structure describing the style of DIALOG-BOX-TEXT."
  (label-style (make-scene2d-label-style) :type scene2d-label-style)
  (lines-spacing 0.0 :type single-float))

(defstruct (dialog-box-text (:include scene2d-box)
                            (:constructor %make-dialog-box-text))
  "A SCENE2D-NODE used to contain and display the text of a DIALOG-BOX."
  (style (make-dialog-box-text-style) :type dialog-box-text-style))

(defun dialog-box-text-string (text)
  "Get the string of DIALOG-BOX-TEXT."
  (apply #'concatenate 'string (reduce (lambda (it acc) (nconc it (cons (string #\Newline) acc)))
                                       (mapcar (lambda (margin)
                                                 (mapcar (compose #'scene2d-label-string #'scene2d-margin-content)
                                                         (scene2d-box-children (scene2d-margin-content margin))))
                                               (scene2d-box-children text))
                                       :initial-value nil
                                       :from-end t)))

(defvar *dialog-box-text*)

(defgeneric ensure-dialog-box-text-label (object)
  (:method ((text string))
    (let ((label-style (dialog-box-text-style-label-style (dialog-box-text-style *dialog-box-text*))))
      (make-scene2d-label :content (make-text :string text :style (scene2d-label-style-text-style label-style)) :style label-style)))
  (:method ((char character))
    (ensure-dialog-box-text-label (string char)))
  (:documentation "Return OBJECT as the label for DIALOG-BOX-TEXT. The returned object doesn't necessarily have to be a SCENE2D-LABEL, but only when the returned object is of that type, can the string of DIALOG-BOX-TEXT be retrieved."))

(defun dialog-box-text-set-content (text string)
  (loop :with *dialog-box-text* := text
        :with style := (dialog-box-text-style text)
        :with flow-box := (make-scene2d-flow-box :size (raylib:copy-vector2 (dialog-box-text-size text)))
        :with hmargin := (/ (text-style-spacing (scene2d-label-style-text-style (dialog-box-text-style-label-style style))) 2.0)
        :initially (setf (scene2d-box-children text) (list flow-box))
        :for char :across string
        :if (member char '(#\Return #\Newline) :test #'eql)
          :unless (scene2d-flow-box-children flow-box)
            :do (scene2d-flow-box-add-child flow-box (make-scene2d-margin :left hmargin :right hmargin :content (ensure-dialog-box-text-label " ")))
          :end :and
          :do (setf flow-box (make-scene2d-flow-box :size (raylib:copy-vector2 (dialog-box-text-size text))))
              (scene2d-box-add-child text flow-box)
        :else
          :do (scene2d-flow-box-add-child flow-box (make-scene2d-margin :left hmargin :right hmargin :content (ensure-dialog-box-text-label char)))
        :finally (scene2d-layout text))
  (loop :with style := (dialog-box-text-style text)
        :with alignment := (make-scene2d-alignment :vertical :start :horizontal :start)
        :for line :in (prog1 (mapcan #'scene2d-box-children (scene2d-box-children text))
                        (setf (dialog-box-text-content text) nil))
        :for cell := (scene2d-box-add-child text (make-scene2d-margin :bottom (dialog-box-text-style-lines-spacing style) :content line))
        :do (setf (scene2d-cell-alignment cell) alignment)
        :finally (scene2d-layout text)))

(defun (setf dialog-box-text-string) (string text)
  "Set the content of TEXT to STRING. STRING doesn't necessarily have to be of type STRING, but it can be a VECTOR composed of elements that specialize method ENSURE-DIALOG-BOX-TEXT-LABEL."
  (dialog-box-text-set-content text string))

(defun make-dialog-box-text (&rest args &key (string "") (size (raylib:make-vector2 :x 100.0 :y 50.0)) &allow-other-keys)
  (remove-from-plistf args :string :size)
  (let ((dialog-box-text (apply #'%make-dialog-box-text :size size args)))
    (setf (dialog-box-text-string dialog-box-text) string)
    dialog-box-text))

(define-scene2d-default-construct-form dialog-box-text (text size style))

(define-scene2d-default-construct-form dialog-box-text-style (label-style lines-spacing))

(defstruct (dialog-box (:include scene2d-container)
                       (:constructor %make-dialog-box))
  "A SCENE2D-NODE that wraps a DIALOG-BOX-TEXT inside it and provides additional functionalities such as vertical scrolling and page indicator."
  (metadata (make-hash-table)))

(defun dialog-box-text (box)
  (gethash 'dialog-box-text (dialog-box-metadata box)))

(defun dialog-box-region (box)
  (gethash 'dialog-box-region (dialog-box-metadata box)))

(defun dialog-box-indicator (box)
  (gethash 'dialog-box-indicator (dialog-box-metadata box)))

(defun dialog-box-string (box)
  "Get the text string of BOX."
  (dialog-box-text-string (dialog-box-text box)))

(defun (setf dialog-box-string) (string box)
  "Set the text content of BOX to STRING. STRING can be any VECTOR whose elements specialize method ENSURE-DIALOG-BOX-TEXT-LABEL."
  (setf (scene2d-size (dialog-box-text box)) (scene2d-size (dialog-box-region box))
        (dialog-box-text-string (dialog-box-text box)) string
        (raylib:vector2-y (dialog-box-text-position (dialog-box-text box))) 0.0))

(define-constant +dialog-box-default-indicator-texture+
    (coerce #(137 80 78 71 13 10 26 10 0 0 0 13 73 72 68 82 0 0 0 7 0 0 0 7 2 3 0 0 0 185
              60 191 64 0 0 0 4 103 65 77 65 0 0 177 143 11 252 97 5 0 0 0 1 115 82 71 66 1
              217 201 44 127 0 0 0 32 99 72 82 77 0 0 122 38 0 0 128 132 0 0 250 0 0 0 128
              232 0 0 117 48 0 0 234 96 0 0 58 152 0 0 23 112 156 186 81 60 0 0 0 9 80 76
              84 69 0 0 0 73 73 73 97 97 97 23 141 53 212 0 0 0 1 116 82 78 83 0 64 230 216
              102 0 0 0 24 73 68 65 84 8 215 99 88 181 130 97 106 4 20 169 38 48 112 54 48
              48 49 0 0 69 168 5 42 80 83 170 209 0 0 0 0 73 69 78 68 174 66 96 130)
            '(simple-array (unsigned-byte 8) (*)))
  :test #'equalp)

(defstruct dialog-box-style
  "A structure describing the style of DIALOG-BOX."
  (dialog-box-text-style (make-dialog-box-text-style) :type dialog-box-text-style)
  (indicator (load-asset 'raylib:texture +dialog-box-default-indicator-texture+ :format :png)))

(defun make-dialog-box (&rest args &key (string "") (style (make-dialog-box-style)) (size (raylib:make-vector2 :x 256.0 :y 30.0)) &allow-other-keys)
  (remove-from-plistf args :string :style :size)
  (let ((dialog-box-text (make-dialog-box-text :position (raylib:make-vector2 :x 0.0 :y 0.0) :size (raylib:copy-vector2 size)
                                               :string string :style (dialog-box-style-dialog-box-text-style style))))
    (multiple-value-bind (group table)
        (scene2d-construct (scene2d-group :children ((scene2d-scroll-region :name dialog-box-region :child dialog-box-text :size (raylib:copy-vector2 size))
                                                     (scene2d-cell :alignment (:start :end)
                                                                   :child (scene2d-container :name dialog-box-indicator :color raylib:+blank+
                                                                                             :child (ensure-scene2d-node (dialog-box-style-indicator style)))
                                                                   :position size :size (0.0 0.0)))))
      (setf (gethash 'dialog-box-text table) dialog-box-text)
      (apply #'%make-dialog-box :content group :metadata table args))))

(define-scene2d-default-construct-form dialog-box (string style size))

(define-scene2d-default-construct-form dialog-box-style (dialog-box-text-style indicator))

(defvar *dialog-box*)

(declaim (type single-float *dialog-box-text-speed*))
(defparameter *dialog-box-text-speed* 40.0)

(defgeneric promise-display-dialog-box-text-label (object)
  (:method ((label scene2d-node))
    (let ((alpha 0.0)
          (duration (/ *dialog-box-text-speed*)))
      (flet ((alpha () alpha)
             ((setf alpha) (value)
               (setf (raylib:color-a (scene2d-color label)) (* (truncate (setf alpha value)) 255))))
        (promise-tween (ute:tween :to (((alpha)) (1.0)) :duration duration :ease #'ute:linear-inout)))))
  (:documentation "Return a PROMISE:PROMISE that is fulfilled after OBJECT is displayed as the label for DIALOG-BOX-TEXT."))

(defun dialog-box-promise-display (box &optional (break-handler (lambda (has-next-p) (declare (ignore has-next-p)) (async))))
  "Make BOX display the text previously set using (SETF DIALOG-BOX-STRING) with the typewriter effect. When the entire text is displayed, the returned PROMISE:PROMISE is fulfilled. BREAK-HANDLER is a function that is called with a parameter indicating whether there is another page or if the text has been fully displayed. It returns a PROMISE:PROMISE, and when this PROMISE:PROMISE is fulfilled, BOX continues to display the remaining content."
  (labels ((line-labels (line) (mapcar #'scene2d-margin-content (scene2d-box-children (scene2d-margin-content line))))
           (hide-line (line) (loop :for label :in (line-labels line) :do (setf (raylib:color-a (scene2d-color label)) 0))))
    (let* ((text (dialog-box-text box))
           (lines (scene2d-box-children (dialog-box-text box)))
           (lines-spacing (dialog-box-text-style-lines-spacing (dialog-box-text-style text)))
           (text-speed *dialog-box-text-speed*)
           (accelerate-text-p nil))
      (setf (raylib:vector2-y (dialog-box-text-position text)) 0.0)
      (loop :for line :in lines
            :do (loop :for label :in (line-labels line)
                      :do (setf (raylib:color-a (scene2d-color label)) 0)))
      (flet ((promise-display-line (line)
               (let ((display-finished-p nil))
                 (unless accelerate-text-p
                   (add-game-loop-hook
                    (lambda ()
                      (when (or (key-pressed-p :a) (key-pressed-p :b))
                        (setf accelerate-text-p t)))
                    :after (lambda (result) (not (or result display-finished-p)))))
                 (async
                   (dolist (label (line-labels line))
                     (let* ((*dialog-box-text-speed* (if accelerate-text-p 60.0 text-speed)))
                       (await (promise-display-dialog-box-text-label label))))
                   (setf display-finished-p t))))
             (promise-next-line (line)
               (symbol-macrolet ((y (raylib:vector2-y (dialog-box-text-position text)))
                                 (height (raylib:vector2-y (scene2d-size line))))
                 (promise-tween (ute:tween :to ((y) ((- y height))) :ease #'ute:linear-inout :duration 0.1)))))
        (loop :with rest-lines :with box-height := (raylib:vector2-y (scene2d-size box))
              :for (line . rest) :on lines
              :summing (raylib:vector2-y (scene2d-size line)) :into height
              :while (<= (- height lines-spacing) box-height)
              :collect line :into initial-lines
              :do (setf rest-lines rest)
              :finally
                 (return
                   (let ((*async-continuation-constructor*
                           (async-special-variable-binder
                            (*dialog-box* *dialog-box-text-speed*)))
                         (*dialog-box* box))
                     (async
                       (dolist (line initial-lines)
                         (await (promise-display-line line)))
                       (nconcf initial-lines rest-lines)
                       (dolist (line rest-lines)
                         (await (funcall break-handler t))
                         (await (promise-next-line line))
                         (hide-line (car initial-lines))
                         (setf initial-lines (cdr initial-lines))
                         (await (promise-display-line line)))
                       (await (funcall break-handler nil))))))))))

(declaim (ftype (function () (values single-float)) dialog-box-indicator-position-y))
(defun dialog-box-indicator-position-y ()
  (let ((y (coerce (raylib:fmod (game-loop-time) 0.5d0) 'single-float)))
    (* (if (< y 0.25) y (- 0.5 y)) -4.0 -2.0)))

(defun dialog-box-promise-confirm (&optional (box *dialog-box*) (display-indicator-p t))
  "Make BOX wait for the user to press a button to continue reading the content. When DISPLAY-INDICATOR-P is non-NIL, a floating page indicator will be displayed during this procedure for prompting purposes."
  (let* ((indicator (dialog-box-indicator box))
         (indicator-y (raylib:vector2-y (scene2d-node-position indicator))))
    (let (confirmedp)
      (when display-indicator-p
        (raylib:copy-color raylib:+white+ (scene2d-node-color indicator)))
      (let ((indicator-position (scene2d-node-position indicator)))
        (add-game-loop-hook
         (lambda ()
           (setf (raylib:vector2-y indicator-position)
                 (+ indicator-y (dialog-box-indicator-position-y)))
           (when confirmedp
             (setf (raylib:vector2-y indicator-position) indicator-y)))
         :before #'not))
      (async
        (loop :until (case (await (promise-pressed-key)) ((:a :b) (setf confirmedp t))))
        (raylib:copy-color raylib:+blank+ (scene2d-node-color indicator))))))

(defun dialog-box-promise-display-confirm (box &optional (last-confirm t))
  "A combination of DIALOG-BOX-PROMISE-DISPLAY and DIALOG-BOX-PROMISE-CONFIRM. LAST-CONFIRM can take the following values:
- T: User confirmation is still required after displaying the entire text.
- :NEXT: User confirmation is still required after displaying the entire text, and the page indicator is shown.
- NIL: The returned PROMISE:PROMISE will be fulfilled directly after displaying the entire text, without requiring user confirmation."
  (dialog-box-promise-display box (lambda (has-next-p)
                                    (if (or last-confirm has-next-p)
                                        (dialog-box-promise-confirm box (or has-next-p (eql last-confirm :next)))
                                        (async)))))
