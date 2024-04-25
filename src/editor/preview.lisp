(in-package #:eon.editor)

(defparameter *preview-function* #'values)

(defmacro with-preview-function (function &body body)
  `(let ((%preview-function ,function)) . ,body))

(defmacro with-received-preview-function (&body body)
  `(let ((%preview-function *preview-function*))
     (flet ((preview (self) (funcall %preview-function self) self))
       (declare (ignorable #'preview))
       . ,body)))

(defmacro with-sent-preview-function ((&optional self place) &body body)
  (with-gensyms (result value lexical)
    `(let ((,lexical ,(if place
                          `(lambda (,value)
                             (setf ,place ,value)
                             (funcall %preview-function ,self))
                          `%preview-function)))
       (let ((*preview-function* ,lexical))
         ,(if self `(let ((,result (progn . ,body))) (preview ,self) ,result) `(progn . ,body))))))

(defstruct (debug-container (:include scene2d-layout))
  (border-color raylib:+red+))

(defmethod scene2d-layout ((container debug-container))
  (call-next-method)
  (setf (scene2d-size container) (scene2d-size (debug-container-content container))))

(defmethod scene2d-draw ((container debug-container) position origin scale rotation tint)
  (declare (ignore origin rotation scale tint))
  (call-next-method)
  (let ((size (debug-container-size container)))
    (let ((x (truncate (raylib:vector2-x position)))
          (y (truncate (raylib:vector2-y position)))
          (width (truncate (raylib:vector2-x size)))
          (height (truncate (raylib:vector2-y size))))
      (let ((child (debug-container-content container)))
        (when (typep child 'scene2d-node)
          (incf x (truncate (raylib:vector2-x (eon::scene2d-node-position child))))
          (incf y (truncate (raylib:vector2-y (eon::scene2d-node-position child))))))
      (raylib:draw-rectangle-lines x y width height (debug-container-border-color container)))))

(defmethod scene2d-construct-form ((type (eql 'debug-container)) &rest args &key child &allow-other-keys)
  (remove-from-plistf args :child)
  (with-gensyms (var)
    `(let ((,var ,child))
       (typecase ,var
         (scene2d-node (make-debug-container :content ,var . ,args))
         (t ,var)))))
