(in-package #:eon)

(defstruct (input-field (:include scene2d-box)))

(defstruct input-field-style
  (label-style (make-scene2d-label-style) :type scene2d-label-style))

(defun input-field-label (field)
  (first (scene2d-box-children field)))

(defun input-field-cursor (field)
  (second (scene2d-box-children field)))

(defun input-field-text (field)
  (scene2d-label-string (input-field-label field)))

(defun (setf input-field-text) (text field)
  (setf (scene2d-label-string (input-field-label field)) text))

(defmethod scene2d-draw ((field input-field) position origin scale rotation tint)
  (setf (raylib:color-a (scene2d-node-color (input-field-cursor field))) (if (< (raylib:fmod (game-loop-time) 1d0) 0.5d0) 255 0))
  (call-next-method))

(defmethod scene2d-construct-form ((type (eql 'input-field)) &rest args &key (string "") (style '(make-input-field-style)) &allow-other-keys)
  (remove-from-plistf args :string :style)
  (let ((style-form (scene2d-argument-construct-form style)))
    (with-gensyms (style field)
      `(let ((,style ,style-form)
             (,field (make-input-field :orientation :horizontal)))
         (scene2d-box-add-child ,field (scene2d-construct (scene2d-label :string ,string :style (input-field-style-label-style ,style))))
         (scene2d-box-add-child ,field (scene2d-construct (scene2d-margin :left 2.0 :right 2.0 :child (scene2d-label :string "_" :style (input-field-style-label-style ,style)))))
         ,field))))

(define-scene2d-default-construct-form input-field-style ())

(defun input-field-promise-line (field)
  (let ((label (input-field-label field)))
    (async
      (loop
        (let ((char (await (promise-pressed-char)))
              (text (scene2d-label-string label)))
          (if (graphic-char-p char)
              (setf (scene2d-label-string label) (concatenate 'string text (string char)))
              (case char
                (#\Rubout (when (plusp (length text)) (setf (scene2d-label-string label) (subseq text 0 (1- (length text))))))
                (#\Return (return text))
                (#\Can (raylib:set-clipboard-text text) (setf (scene2d-label-string label) ""))
                (#\Etx (raylib:set-clipboard-text text))
                (#\Syn (setf (scene2d-label-string label) (concatenate 'string text (raylib:get-clipboard-text))))
                (#\Sub (setf (scene2d-label-string label) ""))))
          (scene2d-layout field))))))
