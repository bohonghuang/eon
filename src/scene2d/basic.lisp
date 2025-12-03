(in-package #:eon)

(cobj:define-global-cobject +vector2-unit-x+ (raylib:make-vector2 :x 1.0 :y 0.0))
(cobj:define-global-cobject +vector2-unit-y+ (raylib:make-vector2 :x 0.0 :y 1.0))
(cobj:define-global-cobject +vector2-zeros+ (raylib:make-vector2 :x 0.0 :y 0.0))
(cobj:define-global-cobject +vector2-ones+ (raylib:make-vector2 :x 1.0 :y 1.0))
(cobj:define-global-cobject +vector2-max+ (raylib:make-vector2 :x most-positive-single-float :y most-positive-single-float))
(cobj:define-global-cobject +vector2-min+ (raylib:make-vector2 :x most-negative-single-float :y most-negative-single-float))

(defgeneric scene2d-draw (drawable position origin scale rotation tint)
  (:method ((list list) position origin scale rotation tint)
    (loop :for drawable :in list
          :do (scene2d-draw drawable position origin scale rotation tint)))
  (:documentation "Draw drawable at POSITION in the 2D scene, scaled by SCALE and rotated by ROTATION at ORIGIN, using color TINT."))

(defun scene2d-draw-simple (drawable
                            &key
                              (position +vector2-zeros+)
                              (origin +vector2-zeros+)
                              (rotation 0.0)
                              (scale +vector2-ones+)
                              (tint raylib:+white+))
  "Like SCENE2D-DRAW, but allow selectively providing drawing parameters, with default values used for parameters not provided."
  (scene2d-draw drawable position origin scale rotation tint))

(declaim (inline %scene2d-draw-texture))
(defun %scene2d-draw-texture (texture source position origin scale rotation tint)
  (declare (optimize (speed 3)
                     (debug 0)
                     (safety 0)))
  (clocally (declare (ctype (:pointer (:struct raylib:texture)) texture)
                     (ctype (:pointer (:struct raylib:rectangle)) source)
                     (ctype (:pointer (:struct raylib:vector2)) position origin scale)
                     (ctype (:pointer (:struct raylib:color)) tint))
    (let ((width (abs (-> source raylib:width)))
          (height (abs (-> source raylib:height)))
          (x (-> position raylib:x))
          (y (-> position raylib:y))
          (origin-x (-> origin raylib:x))
          (origin-y (-> origin raylib:y))
          (scale-x (-> scale raylib:x))
          (scale-y (-> scale raylib:y)))
      (declare (type single-float width height x y origin-x origin-y scale-x scale-y))
      (clet ((dest (foreign-alloca '(:struct raylib:rectangle)))
             (origin (foreign-alloca '(:struct raylib:vector2))))
        (setf (-> dest raylib:x) x
              (-> dest raylib:y) y
              (-> dest raylib:width) (* width scale-x)
              (-> dest raylib:height) (* height scale-y))
        (setf (-> origin raylib:x) (* origin-x scale-x)
              (-> origin raylib:y) (* origin-y scale-y))
        (raylib:%draw-texture-pro texture source dest origin rotation tint)))))

(defmethod scene2d-draw ((texture raylib:texture) position origin scale rotation tint)
  (clet ((region (foreign-alloca '(:struct raylib:rectangle))))
    (setf (-> region raylib:x) 0.0
          (-> region raylib:y) 0.0
          (-> region raylib:width) (coerce (raylib:texture-width texture) 'single-float)
          (-> region raylib:height) (coerce (raylib:texture-height texture) 'single-float))
    (%scene2d-draw-texture (& texture) region (& position) (& origin) (& scale) rotation (& tint))))

(declaim (inline draw-texture-region))
(defun draw-texture-region (texture-region position origin scale rotation tint)
  (%scene2d-draw-texture (& (texture-region-texture texture-region))
                         (& (texture-region-region texture-region))
                         (& position) (& origin) (& scale) rotation (& tint)))

(defmethod scene2d-draw ((texture-region texture-region) position origin scale rotation tint)
  (draw-texture-region texture-region position origin scale rotation tint))

(declaim (inline %scene2d-draw-n-patch))
(defun %scene2d-draw-n-patch (texture info position origin scale rotation tint)
  (declare (optimize (speed 3)
                     (debug 0)
                     (safety 0)))
  (clocally (declare (ctype (:pointer (:struct raylib:texture)) texture)
                     (ctype (:pointer (:struct raylib:vector2)) position origin scale)
                     (ctype (:pointer (:struct raylib:color)) tint)
                     (ctype (:pointer (:struct raylib:n-patch-info)) info))
    (let ((border-width (+ (coerce (-> info raylib:left) 'single-float)
                           (coerce (-> info raylib:right) 'single-float)))
          (border-height (+ (coerce (-> info raylib:top) 'single-float)
                            (coerce (-> info raylib:bottom) 'single-float))))
      (let ((width (+ (-> scale raylib:x) border-width))
            (height (+ (-> scale raylib:y) border-height))
            (x (-> position raylib:x))
            (y (-> position raylib:y))
            (origin-x (-> origin raylib:x))
            (origin-y (-> origin raylib:y)))
        (declare (type single-float width height x y origin-x origin-y))
        (clet ((dest (foreign-alloca '(:struct raylib:rectangle)))
               (origin (foreign-alloca '(:struct raylib:vector2))))
          (setf (-> dest raylib:x) x
                (-> dest raylib:y) y
                (-> dest raylib:width) width
                (-> dest raylib:height) height
                (-> origin raylib:x) (* origin-x (/ width border-width))
                (-> origin raylib:y) (* origin-y (/ height border-height)))
          (raylib:%draw-texture-n-patch texture info dest origin rotation tint))))))

(defmethod scene2d-draw ((n-patch n-patch) position origin scale rotation tint)
  (%scene2d-draw-n-patch (& (n-patch-texture n-patch)) (& (n-patch-info n-patch))
                         (& position) (& origin) (& scale) rotation (& tint)))

(declaim (inline %scene2d-draw-font))
(defun %scene2d-draw-font (font text font-size spacing position origin scale rotation tint)
  (declare (optimize (speed 3)
                     (debug 0)
                     (safety 0)))
  (clocally (declare (ctype (:pointer (:struct raylib:font)) font)
                     (ctype (:pointer (:struct raylib:vector2)) position origin scale)
                     (ctype (:pointer (:struct raylib:color)) tint))
    (let ((origin-x (-> origin raylib:x))
          (origin-y (-> origin raylib:y))
          (scale-x (-> scale raylib:x))
          (scale-y (-> scale raylib:y)))
      (declare (type single-float font-size spacing origin-x origin-y scale-x scale-y))
      (clet ((origin (foreign-alloca '(:struct raylib:vector2))))
        (setf (-> origin raylib:x) (* origin-x scale-x)
              (-> origin raylib:y) (* origin-y scale-y))
        (let ((font-size (* font-size scale-y))
              (spacing (* spacing scale-x)))
          (raylib:%draw-text-pro font text position origin rotation font-size spacing tint))))))

(defmethod scene2d-draw ((text text) position origin scale rotation tint)
  (let ((style (text-style text)))
    (%scene2d-draw-font (& (text-style-font style)) (text-string text) (text-style-size style) (text-style-spacing style)
                        (& position) (& origin) (& scale) rotation (& tint))))

(defgeneric scene2d-size (node)
  (:documentation "Get the size of a 2D scene node (excluding its own scaling)."))

(defun size-rectangle (vector2)
  (raylib:make-rectangle :x 0.0 :y 0.0 :width (raylib:vector2-x vector2) :height (raylib:vector2-y vector2)))

(define-compiler-macro size-rectangle (&whole form vector2)
  (typecase vector2
    ((cons (eql raylib:make-vector2) t)
     (destructuring-bind (&key x y) (cdr vector2)
       `(raylib:make-rectangle :x 0.0 :y 0.0 :width ,x :height ,y)))
    (t form)))

(defun rectangle-position (rectangle)
  (clet ((array (cthe (:pointer (:array (:struct raylib:vector2) 2)) (& rectangle))))
    (raylib::%%%make-vector2 :pointer (& ([] array 0)) :shared-from rectangle)))

(defun rectangle-size (rectangle)
  (clet ((array (cthe (:pointer (:array (:struct raylib:vector2) 2)) (& rectangle))))
    (raylib::%%%make-vector2 :pointer (& ([] array 1)) :shared-from rectangle)))

(defgeneric scene2d-bound (node)
  (:method (object) (size-rectangle (scene2d-size object)))
  (:documentation "Get the bounding rectangle of a 2D scene node (excluding its own position)."))

(defgeneric (setf scene2d-size) (value node)
  (:method (value node) (declare (ignore value node)))
  (:documentation "Set the size of a 2D scene node."))

(defgeneric scene2d-layout (layout)
  (:method (layout) (declare (ignore layout)))
  (:method ((list list)) (loop :for layout :in list :do (scene2d-layout layout)))
  (:documentation "Layout a 2D scene node and its child nodes."))

(defmethod scene2d-size ((texture raylib:texture))
  (raylib:make-vector2 :x (coerce (raylib:texture-width texture) 'single-float)
                       :y (coerce (raylib:texture-height texture) 'single-float)))

(defmethod scene2d-size ((texture-region texture-region))
  (raylib:make-vector2 :x (texture-region-width texture-region)
                       :y (texture-region-height texture-region)))

(defmethod scene2d-size ((n-patch n-patch))
  (raylib:make-vector2 :x (+ (coerce (raylib:n-patch-info-left (n-patch-info n-patch)) 'single-float)
                             (coerce (raylib:n-patch-info-right (n-patch-info n-patch)) 'single-float))
                       :y (+ (coerce (raylib:n-patch-info-top (n-patch-info n-patch)) 'single-float)
                             (coerce (raylib:n-patch-info-bottom (n-patch-info n-patch)) 'single-float))))

(defmethod scene2d-size ((text text))
  (let ((style (text-style text)))
    (raylib:measure-text-ex (text-style-font style) (text-string text) (text-style-size style) (text-style-spacing style))))

(defstruct (scene2d-node (:constructor nil))
  "The base class of 2D scene nodes."
  (position (raylib:make-vector2 :x 0.0 :y 0.0) :type raylib:vector2 :read-only t)
  (origin (raylib:make-vector2 :x 0.0 :y 0.0) :type raylib:vector2 :read-only t)
  (scale (raylib:make-vector2 :x 1.0 :y 1.0) :type raylib:vector2 :read-only t)
  (color (raylib:make-color :r 255 :g 255 :b 255 :a 255) :type raylib:color :read-only t)
  (rotation 0.0 :type single-float))

(setf (fdefinition 'scene2d-position) (fdefinition 'scene2d-node-position)
      (fdefinition 'scene2d-origin) (fdefinition 'scene2d-node-origin)
      (fdefinition 'scene2d-scale) (fdefinition 'scene2d-node-scale)
      (fdefinition 'scene2d-color) (fdefinition 'scene2d-node-color)
      (fdefinition 'scene2d-rotation) (fdefinition 'scene2d-node-rotation))

(define-setf-expander scene2d-rotation (node &environment env)
  (multiple-value-bind (vars vals newval setter getter) (get-setf-expansion node env)
    (declare (ignore newval setter))
    (with-gensyms (store)
      (values vars vals `(,store) `(setf (scene2d-node-rotation ,getter) ,store) `(scene2d-rotation ,getter)))))

(defgeneric ensure-scene2d-node (object &rest args)
  (:method ((null null) &rest args)
    (apply #'make-scene2d-container :content nil args))
  (:method ((node scene2d-node) &key
                                  (position (scene2d-node-position node))
                                  (origin (scene2d-node-origin node))
                                  (scale (scene2d-node-scale node))
                                  (color (scene2d-node-color node))
                                  (rotation (scene2d-node-rotation node)))
    (raylib:copy-vector2 position (scene2d-node-position node))
    (raylib:copy-vector2 origin (scene2d-node-origin node))
    (raylib:copy-vector2 scale (scene2d-node-scale node))
    (raylib:copy-color color (scene2d-node-color node))
    (setf (scene2d-node-rotation node) rotation)
    node)
  (:documentation "Return OBJECT as a SCENE2D-NODE using the construction arguments ARGS."))

(defmethod scene2d-bound :around ((node scene2d-node))
  (let ((origin (scene2d-node-origin node))
        (scale (scene2d-node-scale node))
        (bound (call-next-method)))
    (raylib:make-rectangle
     :x (* (- (raylib:rectangle-x bound) (raylib:vector2-x origin)) (raylib:vector2-x scale))
     :y (* (- (raylib:rectangle-y bound) (raylib:vector2-y origin)) (raylib:vector2-y scale))
     :width (* (raylib:rectangle-width bound) (raylib:vector2-x scale))
     :height (* (raylib:rectangle-height bound) (raylib:vector2-y scale)))))

(defmethod scene2d-bound ((list list))
  (loop :for object :in list
        :for position := (typecase object (scene2d-node (scene2d-node-position object)) (t +vector2-zeros+))
        :and bound := (scene2d-bound object)
        :for x :of-type single-float := (+ (raylib:vector2-x position) (raylib:rectangle-x bound))
        :and y :of-type single-float := (+ (raylib:vector2-y position) (raylib:rectangle-y bound))
        :minimize x :into x-min :of-type single-float
        :minimize y :into y-min :of-type single-float
        :maximize (+ x (raylib:rectangle-width bound)) :into x-max :of-type single-float
        :maximize (+ y (raylib:rectangle-height bound)) :into y-max :of-type single-float
        :finally (return (raylib:make-rectangle :x x-min :y y-min :width (- x-max x-min) :height (- y-max y-min)))))

(defmethod scene2d-size ((list list))
  (if list (rectangle-size (scene2d-bound list)) (raylib:vector2-zero)))

(defstruct (scene2d-container (:include scene2d-node))
  "A SCENE2D-NODE that can contain other drawables as its children."
  (content nil))

(defmethod scene2d-layout ((container scene2d-container))
  (scene2d-layout (scene2d-container-content container)))

(defmethod scene2d-bound ((node scene2d-container))
  (if (scene2d-container-content node)
      (scene2d-bound (scene2d-container-content node))
      (raylib:make-rectangle :x 0.0 :y 0.0 :width 0.0 :height 0.0)))

(defmethod scene2d-size ((container scene2d-container))
  (scene2d-size (scene2d-container-content container)))

(defmacro with-scene2d-container-transform ((container (position origin scale rotation tint)) &body body)
  (with-gensyms (target-position
                 target-origin
                 target-scale
                 target-color
                 target-rotation
                 offset-position
                 offset-origin
                 offset-scale
                 offset-color
                 offset-rotation
                 original-position
                 original-origin
                 original-scale
                 original-color)
    `(let ((,target-position (& (scene2d-container-position ,container)))
           (,target-origin (& (scene2d-container-origin ,container)))
           (,target-scale (& (scene2d-container-scale ,container)))
           (,target-color (& (scene2d-container-color ,container)))
           (,target-rotation (scene2d-container-rotation ,container))
           (,offset-position (& ,position))
           (,offset-origin (& ,origin))
           (,offset-scale (& ,scale))
           (,offset-color (& ,tint))
           (,offset-rotation ,rotation))
       (declare (type single-float ,target-rotation ,offset-rotation))
       (clocally (declare (ctype (:pointer (:struct raylib:vector2))
                                 ,target-position ,target-origin ,target-scale
                                 ,offset-position ,offset-origin ,offset-scale)
                          (ctype (:pointer (:struct raylib:color))
                                 ,target-color ,offset-color))
         (clet ((,original-position (foreign-alloca '(:struct raylib:vector2)))
                (,original-origin (foreign-alloca '(:struct raylib:vector2)))
                (,original-scale (foreign-alloca '(:struct raylib:vector2)))
                (,original-color (foreign-alloca '(:struct raylib:color))))
           (csetf ([] ,original-position) ([] ,target-position)
                  ([] ,original-origin) ([] ,target-origin)
                  ([] ,original-scale) ([] ,target-scale)
                  ([] ,original-color) ([] ,target-color))
           (raylib:%vector2-add ,target-position ,target-position ,offset-position)
           (raylib:%vector2-multiply ,target-scale ,target-scale ,offset-scale)
           (raylib:%vector2-add ,target-origin ,target-origin ,offset-origin)
           (raylib:%color-tint ,target-color ,target-color ,offset-color)
           (let ((,position (scene2d-container-position ,container))
                 (,origin (scene2d-container-origin ,container))
                 (,scale (scene2d-container-scale ,container))
                 (,tint (scene2d-container-color ,container))
                 (,rotation (+ ,target-rotation ,offset-rotation)))
             ,@body)
           (csetf ([] ,target-position) ([] ,original-position)
                  ([] ,target-origin) ([] ,original-origin)
                  ([] ,target-scale) ([] ,original-scale)
                  ([] ,target-color) ([] ,original-color)))))))

(defmethod scene2d-draw ((container scene2d-container) position origin scale rotation tint)
  (scene2d-draw (scene2d-container-content container) position origin scale rotation tint))

(defmethod scene2d-draw :around ((container scene2d-container) position origin scale rotation tint)
  (with-scene2d-container-transform (container (position origin scale rotation tint))
    (call-next-method container position origin scale rotation tint)))

(defgeneric scene2d-traverse (operation node)
  (:method-combination or :most-specific-last)
  (:method or ((operation function) (node t)))
  (:method or ((operation function) (node scene2d-node))
    (funcall operation node))
  (:method or ((operation function) (nodes list))
    (loop :for node :in nodes :thereis (scene2d-traverse operation node)))
  (:method or ((operation function) (node scene2d-container))
    (when-let ((result (scene2d-traverse operation (scene2d-container-content node))))
      (funcall operation node result))))

(defgeneric scene2d-node-position- (child parent)
  (:method ((child scene2d-node) (parent scene2d-container))
    (let ((operation (lambda (node &optional result)
                       (if result
                           (raylib:vector2-add (scene2d-node-position node) result)
                           (when (eq node child) (scene2d-node-position node))))))
      (declare (dynamic-extent operation))
      (scene2d-traverse operation parent))))

(defgeneric scene2d-node-origin- (child parent)
  (:method ((child scene2d-node) (parent scene2d-container))
    (let ((operation (lambda (node &optional result)
                       (if result
                           (raylib:vector2-add (scene2d-node-origin node) result)
                           (when (eq node child) (scene2d-node-origin node))))))
      (declare (dynamic-extent operation))
      (scene2d-traverse operation parent))))

(defgeneric scene2d-node-scale- (child parent)
  (:method ((child scene2d-node) (parent scene2d-container))
    (let ((operation (lambda (node &optional result)
                       (if result
                           (raylib:vector2-multiply (scene2d-node-scale node) result)
                           (when (eq node child) (scene2d-node-scale node))))))
      (declare (dynamic-extent operation))
      (scene2d-traverse operation parent))))

(defgeneric scene2d-node-color- (child parent)
  (:method ((child scene2d-node) (parent scene2d-container))
    (let ((operation (lambda (node &optional result)
                       (if result
                           (raylib:color-tint (scene2d-node-color node) result)
                           (when (eq node child) (scene2d-node-color node))))))
      (declare (dynamic-extent operation))
      (scene2d-traverse operation parent))))

(defgeneric scene2d-node-rotation- (child parent)
  (:method ((child scene2d-node) (parent scene2d-container))
    (let ((operation (lambda (node &optional result)
                       (if result
                           (locally (declare (type single-float result))
                             (+ (scene2d-node-rotation node) result))
                           (when (eq node child) (scene2d-node-rotation node))))))
      (declare (dynamic-extent operation))
      (scene2d-traverse operation parent))))

(setf (fdefinition 'scene2d-position) (fdefinition 'scene2d-node-position)
      (fdefinition 'scene2d-origin) (fdefinition 'scene2d-node-origin)
      (fdefinition 'scene2d-scale) (fdefinition 'scene2d-node-scale)
      (fdefinition 'scene2d-color) (fdefinition 'scene2d-node-color)
      (fdefinition 'scene2d-rotation) (fdefinition 'scene2d-node-rotation)
      (fdefinition 'scene2d-position-) (fdefinition 'scene2d-node-position-)
      (fdefinition 'scene2d-origin-) (fdefinition 'scene2d-node-origin-)
      (fdefinition 'scene2d-scale-) (fdefinition 'scene2d-node-scale-)
      (fdefinition 'scene2d-color-) (fdefinition 'scene2d-node-color-)
      (fdefinition 'scene2d-rotation-) (fdefinition 'scene2d-node-rotation-))

(defstruct scene2d-alignment
  "A structure defining the 2D alignment of a SCENE2D-NODE."
  (vertical :center :type (member :start :center :end))
  (horizontal :center :type (member :start :center :end)))

(defstruct (scene2d-layout (:include scene2d-container))
  "A SCENE2D-NODE that serves as the base class for all layouts and contains a size field independent of its child nodes."
  (size (raylib:make-vector2 :x 0.0 :y 0.0) :type raylib:vector2 :read-only t))

(defmethod scene2d-size ((layout scene2d-layout))
  (scene2d-layout-size layout))

(defmethod (setf scene2d-size) (value (layout scene2d-layout))
  (raylib:copy-vector2 value (scene2d-layout-size layout)))

(defmethod scene2d-bound ((layout scene2d-layout))
  (size-rectangle (scene2d-size layout)))

(defstruct (scene2d-cell (:include scene2d-layout))
  "A SCENE2D-LAYOUT with a specified size, where its child node can be aligned with it using the specified alignment."
  (alignment (make-scene2d-alignment) :type scene2d-alignment))

(defmacro subst-swap (tree &body pairs)
  (with-gensyms (temp)
    `(progn . ,(loop :for (a b) :in pairs
                     :collect `(setf ,tree (nsubst ',temp ',a ,tree)
                                     ,tree (nsubst ',a ',b ,tree)
                                     ,tree (nsubst ',b ',temp ,tree))))))

(defmethod scene2d-layout ((cell scene2d-cell))
  (call-next-method)
  (when-let ((child (scene2d-cell-content cell)))
    (let ((size (scene2d-size cell))
          (bound (scene2d-bound child)))
      (macrolet ((symmetric-impl (horizontal-impl &aux (vertical-impl (copy-tree horizontal-impl)))
                   (subst-swap vertical-impl
                     (raylib:vector2-x raylib:vector2-y)
                     (raylib:rectangle-x raylib:rectangle-y)
                     (raylib:rectangle-width raylib:rectangle-height)
                     (scene2d-alignment-horizontal scene2d-alignment-vertical))
                   `(progn ,horizontal-impl ,vertical-impl)))
        (symmetric-impl
         (progn
           (ecase (scene2d-alignment-horizontal (scene2d-cell-alignment cell))
             (:start (setf (raylib:vector2-x (scene2d-node-position child)) (- (raylib:rectangle-x bound))))
             (:center (setf (raylib:vector2-x (scene2d-node-position child)) (- (/ (- (raylib:vector2-x size) (raylib:rectangle-width bound)) 2.0) (raylib:rectangle-x bound))))
             (:end (setf (raylib:vector2-x (scene2d-node-position child)) (- (raylib:vector2-x size) (raylib:rectangle-x bound) (raylib:rectangle-width bound)))))
           (incf (raylib:vector2-x (scene2d-node-position child)) (* (raylib:vector2-x (scene2d-node-scale child)) (raylib:vector2-x (scene2d-node-origin child))))))))))

(defstruct (scene2d-margin (:include scene2d-container)
                           (:constructor %make-scene2d-margin))
  "A SCENE2D-LAYOUT that provides margins around its child node."
  (lower (raylib:make-vector2 :x 0.0 :y 0.0) :read-only t)
  (upper (raylib:make-vector2 :x 0.0 :y 0.0) :read-only t))

(defun make-scene2d-margin (&rest args &key top bottom left right &allow-other-keys)
  (remove-from-plistf args :top :bottom :left :right)
  (let ((margin (apply #'%make-scene2d-margin args)))
    (with-accessors ((lower scene2d-margin-lower) (upper scene2d-margin-upper))
        margin
      (setf (raylib:vector2-x lower) (or left 0.0)
            (raylib:vector2-y lower) (or top 0.0)
            (raylib:vector2-x upper) (or right 0.0)
            (raylib:vector2-y upper) (or bottom 0.0))
      margin)))

(defmethod scene2d-layout ((margin scene2d-margin))
  (call-next-method)
  (when-let ((child (scene2d-margin-content margin)))
    (raylib:copy-vector2
     (raylib:vector2-subtract
      (scene2d-margin-lower margin)
      (rectangle-position (scene2d-bound child)))
     (scene2d-node-position child))))

(defmethod scene2d-size ((margin scene2d-margin))
  (raylib:vector2-add
   (scene2d-size (scene2d-margin-content margin))
   (raylib:vector2-add
    (scene2d-margin-lower margin)
    (scene2d-margin-upper margin))))

(defmethod scene2d-bound ((margin scene2d-margin))
  (size-rectangle (scene2d-size margin)))

(defstruct (scene2d-box (:include scene2d-layout))
  "A SCENE2D-LAYOUT that can arrange its child nodes in a specified orientation."
  (orientation :vertical :type (member :vertical :horizontal)))

(defmethod scene2d-layout ((box scene2d-box))
  (macrolet ((symmetric-impl (&body vertical-impl &aux (horizontal-impl (copy-tree vertical-impl)))
               (subst-swap horizontal-impl
                 (raylib:vector2-x raylib:vector2-y)
                 (raylib:rectangle-width raylib:rectangle-height)
                 (width height)
                 (x y))
               `(ecase (scene2d-box-orientation box)
                  (:vertical . ,vertical-impl)
                  (:horizontal . ,horizontal-impl))))
    (symmetric-impl
     (loop :for cell :in (scene2d-box-content box)
           :for content := (scene2d-cell-content cell)
           :do (scene2d-layout content)
           :maximize (raylib:rectangle-width (scene2d-bound content)) :into width :of-type single-float
           :finally (setf (raylib:vector2-x (scene2d-box-size box)) (max width 0.0)))
     (loop :with width := (raylib:vector2-x (scene2d-box-size box))
           :for cell :in (scene2d-box-content box)
           :for content := (scene2d-cell-content cell)
           :do (setf (raylib:vector2-x (scene2d-cell-size cell)) width
                     (raylib:vector2-y (scene2d-cell-size cell)) (raylib:rectangle-height (scene2d-bound content)))
           :do (scene2d-layout cell)
               (setf (raylib:vector2-x (scene2d-cell-position cell)) 0.0
                     (raylib:vector2-y (scene2d-cell-position cell)) height)
           :summing (raylib:vector2-y (scene2d-cell-size cell)) :into height :of-type single-float
           :finally (setf (raylib:vector2-y (scene2d-box-size box)) height)))))

(defun scene2d-box-children (box)
  "Get the children of BOX."
  (mapcar #'scene2d-cell-content (scene2d-box-content box)))

(defun scene2d-box-add-child (box child &optional (alignment (if-let ((cell (lastcar (scene2d-box-content box))))
                                                               (ecase (scene2d-box-orientation box)
                                                                 (:vertical (scene2d-alignment-horizontal (scene2d-cell-alignment cell)))
                                                                 (:horizontal (scene2d-alignment-vertical (scene2d-cell-alignment cell))))
                                                               :center)))
  "Add CHILD to BOX as its last child. ALIGNMENT is used to specify the alignment of the added child's cell."
  (let ((cell (make-scene2d-cell
               :content child
               :alignment (etypecase alignment
                            (scene2d-alignment alignment)
                            ((member :start :center :end)
                             (ecase (scene2d-box-orientation box)
                               (:vertical (make-scene2d-alignment :horizontal alignment))
                               (:horizontal (make-scene2d-alignment :vertical alignment))))))))
    (nconcf (scene2d-box-content box) (list cell))
    cell))

(defun (setf scene2d-box-children) (children box)
  "Set the children of BOX."
  (setf (scene2d-box-content box) nil)
  (mapc (curry #'scene2d-box-add-child box) children))

(defun scene2d-box-remove-child (box child)
  "Remove CHILD from BOX."
  (when-let ((cell (find child (scene2d-box-content box) :key #'scene2d-cell-content)))
    (deletef (scene2d-box-content box) cell)))

(defstruct (scene2d-nine-patch (:include scene2d-layout))
  "A SCENE2D-NODE for an N-PATCH.")

(defmethod ensure-scene2d-node ((nine-patch n-patch) &rest args)
  (apply #'make-scene2d-nine-patch :content nine-patch args))

(defun ensure-scene2d-node-origin-at-center (node)
  (raylib:copy-vector2
   (raylib:copy-vector2
    (raylib:vector2-scale (scene2d-size node) 0.5)
    (scene2d-node-origin node))
   (scene2d-node-position node))
  node)

(defmethod scene2d-draw ((nine-patch scene2d-nine-patch) position origin scale rotation tint)
  (let ((size (scene2d-nine-patch-size nine-patch)))
    (let ((width (raylib:vector2-x size))
          (height (raylib:vector2-y size)))
      (raylib:%vector2-multiply (& size) (& size) (& scale))
      (scene2d-draw (scene2d-nine-patch-content nine-patch) position origin size rotation tint)
      (setf (raylib:vector2-x size) width
            (raylib:vector2-y size) height))))

(defmethod scene2d-size ((nine-patch scene2d-nine-patch))
  (raylib:vector2-add
   (scene2d-size (scene2d-nine-patch-content nine-patch))
   (scene2d-nine-patch-size nine-patch)))

(defmethod (setf scene2d-size) (value (nine-patch scene2d-nine-patch))
  (raylib:%vector2-subtract (& (scene2d-nine-patch-size nine-patch)) (& value) (& (scene2d-size (scene2d-nine-patch-content nine-patch)))))

(define-constant +scene2d-window-default-background-texture+
    (coerce #(137 80 78 71 13 10 26 10 0 0 0 13 73 72 68 82 0 0 0 13 0 0 0 13 8 4 0 0 0 216
              226 44 247 0 0 0 4 103 65 77 65 0 0 177 143 11 252 97 5 0 0 0 1 115 82 71 66
              1 217 201 44 127 0 0 0 32 99 72 82 77 0 0 122 38 0 0 128 132 0 0 250 0 0 0
              128 232 0 0 117 48 0 0 234 96 0 0 58 152 0 0 23 112 156 186 81 60 0 0 0 110
              73 68 65 84 24 211 117 144 43 18 128 48 12 68 95 152 122 20 166 10 133 229 22
              40 142 219 139 112 131 170 26 84 79 16 12 253 16 32 102 103 247 77 54 51 17
              88 65 177 35 7 178 130 250 23 73 32 14 245 76 140 15 144 129 164 3 96 64 241
              174 5 59 51 145 80 189 107 96 3 22 168 112 40 104 54 218 161 104 180 43 12
              240 119 139 46 236 10 179 9 243 189 37 73 225 252 248 134 252 255 240 2 185
              129 24 166 193 123 122 175 0 0 0 0 73 69 78 68 174 66 96 130)
            '(simple-array (unsigned-byte 8) (*)))
  :test #'equalp)

(defun scene2d-window-default-background ()
  (ensure-scene2d-node
   (make-n-patch :texture (load-asset 'raylib:texture +scene2d-window-default-background-texture+ :format :png)
                 :body (raylib:make-rectangle :x 6.0 :y 6.0 :width -6.0 :height -6.0))))

(defstruct scene2d-window-style
  (background (scene2d-window-default-background)))

(defstruct (scene2d-window (:include scene2d-container))
  "A SCENE2D-CONTAINER that adjusts its background size during layout to accommodate its child.")

(defun scene2d-window-child (window)
  "Get the child of WINDOW."
  (second (scene2d-window-content window)))

(defun (setf scene2d-window-child) (child window)
  "Set the child of WINDOW."
  (setf (second (scene2d-window-content window)) child))

(defun scene2d-window-background (window)
  "Get the background of WINDOW."
  (first (scene2d-window-content window)))

(defun (setf scene2d-window-background) (background window)
  "Set the background of WINDOW."
  (setf (first (scene2d-window-content window)) background))

(defgeneric scene2d-window-layout (child background)
  (:method (child background)
    (setf (scene2d-size background) (scene2d-size child)))
  (:documentation "Layout BACKGROUND fit the content of CHILD."))

(defmethod scene2d-layout ((window scene2d-window))
  (scene2d-layout (scene2d-window-child window))
  (scene2d-window-layout (scene2d-window-child window) (scene2d-window-background window))
  (scene2d-layout (scene2d-window-background window)))

(defmethod scene2d-window-layout (child (scene2d-nine-patch scene2d-nine-patch))
  (raylib:copy-vector2 (scene2d-size child) (scene2d-nine-patch-size scene2d-nine-patch)))

(defmethod scene2d-window-layout ((child scene2d-node) (scene2d-nine-patch scene2d-nine-patch))
  (let ((nine-patch (scene2d-nine-patch-content scene2d-nine-patch)))
    (call-next-method)
    (setf (raylib:vector2-x (scene2d-node-position child))
          (coerce (raylib:n-patch-info-left (n-patch-info nine-patch)) 'single-float)
          (raylib:vector2-y (scene2d-node-position child))
          (coerce (raylib:n-patch-info-top (n-patch-info nine-patch)) 'single-float))))

(defstruct (scene2d-flow-box (:include scene2d-box))
  "A SCENE2D-LAYOUT that fixes its size in one dimension, fill a portion of its children based on that dimension, and then arrange the remaining children by increasing the size in another dimension, repeating this process."
  (alignment (make-scene2d-alignment) :type scene2d-alignment))

(defun scene2d-flow-box-children (flow-box)
  "Get the children of FLOW-BOX."
  (mapcan #'scene2d-box-children (scene2d-box-children flow-box)))

(defun scene2d-flow-box-cells (flow-box)
  (mappend #'scene2d-box-content (scene2d-box-children flow-box)))

(defun scene2d-flow-box-make-box (flow-box)
  (let ((box (make-scene2d-box :orientation (ecase (scene2d-flow-box-orientation flow-box)
                                              (:vertical :horizontal)
                                              (:horizontal :vertical)))))
    (scene2d-box-add-child flow-box box (scene2d-flow-box-alignment flow-box))
    box))

(defun scene2d-flow-box-add-child (flow-box child)
  "Add CHILD to FLOW-BOX as its last child."
  (scene2d-box-add-child (scene2d-flow-box-make-box flow-box) child (scene2d-flow-box-alignment flow-box)))

(defmethod scene2d-layout ((flow-box scene2d-flow-box))
  (macrolet ((symmetric-impl (&body vertical-impl &aux (horizontal-impl (copy-tree vertical-impl)))
               (subst-swap horizontal-impl
                 (raylib:vector2-x raylib:vector2-y)
                 (raylib:rectangle-width raylib:rectangle-height)
                 (width height)
                 (max-width max-height)
                 (x y)
                 (:horizontal :vertical))
               `(ecase (scene2d-box-orientation flow-box)
                  (:vertical . ,vertical-impl)
                  (:horizontal . ,horizontal-impl))))
    (symmetric-impl
     (loop :with alignment :of-type scene2d-alignment := (scene2d-flow-box-alignment flow-box)
           :and max-width :of-type single-float := (raylib:vector2-x (scene2d-flow-box-size flow-box))
           :and children :of-type list := (scene2d-flow-box-cells flow-box)
           :with conses := (loop :for box :in (scene2d-box-children flow-box) :nconc (loop :for cons :on (shiftf (scene2d-box-content box) nil) :collect cons))
           :initially (assert (= (length conses) (length children)))
           :for box :in (scene2d-box-children flow-box)
           :while children
           :do (loop :for cell := (first children)
                     :for child := (scene2d-cell-content cell)
                     :do (setf (scene2d-cell-alignment cell) alignment) (scene2d-layout child)
                     :summing (raylib:rectangle-width (scene2d-bound child)) :into width :of-type single-float
                     :until (and (scene2d-box-content box) (> width max-width))
                     :do (nconcf (scene2d-box-content box) (let ((cons (pop conses)))
                                                             (setf (car cons) (pop children) (cdr cons) nil)
                                                             cons))
                     :while children))))
  (call-next-method))

(defstruct (scene2d-coordinate-truncator (:include scene2d-container))
  "A SCENE2D-CONTAINER that ensure the world coordinate values of its child node are always integers.")

(defmethod scene2d-draw ((truncator scene2d-coordinate-truncator) position origin scale rotation tint)
  (declare (ignore origin rotation scale tint))
  (setf (raylib:vector2-x position) (ftruncate (raylib:vector2-x position))
        (raylib:vector2-y position) (ftruncate (raylib:vector2-y position))
        (raylib:vector2-x origin) (ftruncate (raylib:vector2-x origin))
        (raylib:vector2-y origin) (ftruncate (raylib:vector2-y origin)))
  (call-next-method))

(defstruct scene2d-label-style
  "A structure used to describe the style of a SCENE2D-LABEL, including text style, text color, outline color, and shadow color."
  (text-style (make-text-style) :type text-style)
  (color (raylib:make-color :r 74 :g 73 :b 74 :a 255) :type raylib:color)
  (shadow (raylib:make-color :r 214 :g 215 :b 206 :a 255) :type (or raylib:color null))
  (outline nil :type (or raylib:color null)))

(defstruct (scene2d-label (:include scene2d-coordinate-truncator))
  "A SCENE2D-NODE capable of displaying specified text using a specific style."
  (style (make-scene2d-label-style) :type scene2d-label-style))

(defmethod ensure-scene2d-node ((text text) &rest args)
  (apply #'make-scene2d-label :content text args))

(defun scene2d-label-string (label)
  "Get the text string of LABEL."
  (if-let ((text (scene2d-label-content label)))
    (text-string text) ""))

(defun (setf scene2d-label-string) (text label)
  "Set the text string of LABEL."
  (if (scene2d-label-content label)
      (setf (text-string (scene2d-label-content label)) text)
      (setf (scene2d-label-content label) (make-text :string text :style (scene2d-label-style-text-style (scene2d-label-style label))))))

(defmethod scene2d-draw ((label scene2d-label) position origin scale rotation tint)
  (when (plusp (raylib:color-a tint))
    (setf (raylib:vector2-x position) (ftruncate (raylib:vector2-x position))
          (raylib:vector2-y position) (ftruncate (raylib:vector2-y position)))
    (let* ((text (scene2d-label-content label))
           (style (text-style text)))
      (with-foreign-string (text-string (text-string text))
        (clet ((color (foreign-alloca '(:struct raylib:color))))
          (let ((outline-color (scene2d-label-style-outline (scene2d-label-style label)))
                (shadow-color (scene2d-label-style-shadow (scene2d-label-style label))))
            (assert (not (and outline-color shadow-color)))
            (when-let ((tint-color (or outline-color shadow-color)))
              (raylib:%color-tint color (& tint-color) (& tint))
              (loop :with offset-src :of-type single-float := (if outline-color -1.0 0.0)
                    :and offset-dest :of-type single-float := 1.0
                    :with position-x :of-type single-float := (raylib:vector2-x position)
                    :and position-y :of-type single-float := (raylib:vector2-y position)
                    :for offset-y :of-type single-float :from offset-src :to offset-dest
                    :do (loop :initially (setf (raylib:vector2-y position) (+ position-y offset-y))
                              :for offset-x :of-type single-float :from offset-src :to offset-dest
                              :do (setf (raylib:vector2-x position) (+ position-x offset-x))
                              :unless (and (zerop (raylib:vector2-x position)) (zerop (raylib:vector2-y position)))
                                :do (%scene2d-draw-font (& (text-style-font style)) text-string (text-style-size style) (text-style-spacing style)
                                                        (& position) (& origin) (& scale) rotation color))
                    :finally (setf (raylib:vector2-x position) position-x
                                   (raylib:vector2-y position) position-y))))
          (let ((text-color (scene2d-label-style-color (scene2d-label-style label))))
            (raylib:%color-tint color (& text-color) (& tint))
            (%scene2d-draw-font (& (text-style-font style)) text-string (text-style-size style) (text-style-spacing style)
                                (& position) (& origin) (& scale) rotation color)))))))

(defstruct (scene2d-scissor (:include scene2d-layout))
  "A SCENE2D-LAYOUT capable of clipping the content of its child node beyond its bound.")

(defmethod scene2d-draw ((scissor scene2d-scissor) position origin scale rotation tint)
  (raylib:with-scissor-mode ((floor (raylib:vector2-x position))
                             (floor (raylib:vector2-y position))
                             (floor (raylib:vector2-x (scene2d-scissor-size scissor)))
                             (floor (raylib:vector2-y (scene2d-scissor-size scissor))))
    (call-next-method)))

(defstruct (scene2d-image (:include scene2d-container))
  "A SCENE2D-NODE used for displaying image (TEXTURE-REGION).")

(defmethod ensure-scene2d-node ((texture-region texture-region) &rest args)
  (apply #'make-scene2d-image :content texture-region args))

(defmethod ensure-scene2d-node ((texture raylib:texture) &rest args)
  (apply #'ensure-scene2d-node (make-texture-region :texture texture) args))

(defmethod ensure-scene2d-node ((image raylib:image) &rest args)
  (apply #'ensure-scene2d-node (load-asset 'raylib:texture image) args))

(defun scene2d-image-tween-frames (image frames
                                   &key
                                     (duration 1.0)
                                     (repeat nil)
                                     (yoyop nil)
                                     (interval 0.0 interval-p)
                                     (restore-frame-p nil))
  "Return a TWEEN that sets the FRAMES as the content of IMAGE sequentially at INTERVAL or within DURATION, repeating this process REPEAT times. If RESTORE-FRAME-P is non-NIL, restore the original content of IMAGE after this process ends."
  (declare (type single-float duration interval))
  (tween-iteration-in-place
      ((scene2d-image-content image) frames)
      :duration (if interval-p (* interval (length frames)) duration)
      :repeat (:count (or repeat 0) :yoyop yoyop)
      :ease #'ute:linear-inout
      :restore-place-p restore-frame-p))

(defstruct (scene2d-group (:include scene2d-container)
                          (:constructor %make-scene2d-group))
  "A SCENE2D-CONTAINER that can accommodate multiple child nodes, with its size determined by the boundary formed by its child nodes. The child nodes will be rendered sequentially, and the last child node will be rendered on top.")

(defun make-scene2d-group (&rest args)
  (apply #'%make-scene2d-group :content (make-scene2d-container) args))

(defun scene2d-group-children (group)
  "Get the children of GROUP."
  (scene2d-container-content (scene2d-group-content group)))

(defun scene2d-group-add-child (group child)
  "Add CHILD to GROUP."
  (nconcf (scene2d-container-content (scene2d-group-content group)) (list child)))

(defun scene2d-group-remove-child (group child &key (from-end nil) (test #'eq) (key #'identity))
  "Remove CHILD from GROUP."
  (deletef (scene2d-container-content (scene2d-group-content group)) child :from-end from-end :test test :key key))

(defmethod scene2d-bound ((group scene2d-group))
  (let ((bound (call-next-method)))
    (incf (raylib:rectangle-width bound) (max (raylib:rectangle-x bound) 0.0))
    (incf (raylib:rectangle-height bound) (max (raylib:rectangle-y bound) 0.0))
    (setf (raylib:rectangle-x bound) 0.0 (raylib:rectangle-y bound) 0.0)
    bound))

(defmethod scene2d-size ((group scene2d-group))
  (rectangle-size (scene2d-bound (scene2d-group-content group))))

(defmethod scene2d-layout ((group scene2d-group))
  (call-next-method)
  (raylib:copy-vector2
   (raylib:vector2-clamp
    (raylib:vector2-negate
     (rectangle-position (scene2d-bound (scene2d-group-content group))))
    +vector2-zeros+ +vector2-max+)
   (scene2d-container-position (scene2d-group-content group))))

(defstruct scene2d-dimensions
  "A structure representing the dimensions of a specific SCENE2D-NODE."
  (rows 2 :type fixnum)
  (columns 1 :type fixnum))

(defstruct (scene2d-max-cell (:include scene2d-cell))
  "A SCENE2D-LAYOUT that behaves like a SCENE2D-CELL when its specified size is larger than its child; otherwise, behaves like a SCENE2D-CONTAINER, allowing the child's size to determine its own size.")

(defmethod scene2d-size ((cell scene2d-max-cell))
  (raylib:vector2-clamp
   (rectangle-size (scene2d-bound (scene2d-max-cell-content cell)))
   (scene2d-max-cell-size cell)
   +vector2-max+))

(defstruct (scene2d-table-cell (:include scene2d-max-cell))
  "A SCENE2D-MAX-CELL used internally within a SCENE2D-TABLE."
  (span 1 :type positive-fixnum))

(defstruct (scene2d-table (:include scene2d-box))
  "A SCENE2D-LAYOUT capable of maintaining alignment of its children in rows and columns (defaulting to center alignment).")

(defun scene2d-table-newline (table)
  "Create a new line for TABLE to place children. Note that the meaning of \"line\" here refers to either a row or a column, depending on the orientation of the TABLE."
  (let* ((box (make-scene2d-box :orientation (ecase (scene2d-table-orientation table)
                                               (:vertical :horizontal)
                                               (:horizontal :vertical))))
         (cell (scene2d-box-add-child table box)))
    (setf (scene2d-cell-alignment cell) (make-scene2d-alignment :vertical :start :horizontal :start))
    cell))

(defun scene2d-table-add-child (table child)
  "Add CHILD to the current line of TABLE."
  (let ((box (lastcar (scene2d-box-children table)))
        (cell (make-scene2d-table-cell :content child)))
    (scene2d-box-add-child box cell) cell))

(defun scene2d-table-children (table)
  "Return the children of TABLE as a list of lists."
  (mapcar (lambda (line)
            (mapcar #'scene2d-table-cell-content (scene2d-box-children line)))
          (scene2d-box-children table)))

(defmethod scene2d-layout ((table scene2d-table))
  (macrolet ((symmetric-impl (&body vertical-impl &aux (horizontal-impl (copy-tree vertical-impl)))
               (subst-swap horizontal-impl
                 (raylib:vector2-x raylib:vector2-y)
                 (cols rows)
                 (col-width row-height)
                 (col-widths row-heights)
                 (col-start row-start)
                 (col-end row-end)
                 (cell-width cell-height)
                 (delta-width delta-height)
                 (zero-cols zero-rows))
               `(ecase (scene2d-table-orientation table)
                  (:vertical . ,vertical-impl)
                  (:horizontal . ,horizontal-impl))))
    (symmetric-impl
     (loop :for child :in (scene2d-box-children table)
           :do (loop :for cell :in (scene2d-box-children child)
                     :do (raylib:copy-vector2 +vector2-zeros+ (scene2d-table-cell-size cell))))
     (call-next-method)
     (loop :with cols := (loop :for child :in (scene2d-box-children table)
                               :maximize (loop :for cell :in (scene2d-box-children child)
                                               :summing (scene2d-table-cell-span cell)))
           :with col-widths := (make-array cols :element-type 'single-float :initial-element 0.0)
           :for span :from 1 :to cols
           :do (loop :for child :in (scene2d-box-children table)
                     :do (loop :for cell :in (scene2d-box-children child)
                               :for box-cell :in (scene2d-box-content child)
                               :for cell-span := (scene2d-table-cell-span cell)
                               :for col-start := 0 :then (+ col-start (scene2d-table-cell-span cell))
                               :for col-end := (+ col-start cell-span)
                               :for col-width := (loop :for col :from col-start :below col-end
                                                       :summing (aref col-widths col))
                               :for cell-width := (raylib:vector2-x (scene2d-cell-size box-cell))
                               :when (= span cell-span)
                                 :when (< col-width cell-width)
                                   :do (loop :with delta-width := (- cell-width col-width)
                                             :for col :from col-start :below col-end
                                             :when (zerop (aref col-widths col))
                                               :collect col :into zero-cols
                                               :and :count 1 :into zeros
                                             :finally
                                                (if zero-cols
                                                    (loop :for col :in zero-cols
                                                          :do (setf (aref col-widths col) (/ delta-width zeros)))
                                                    (loop :with ratio := (/ cell-width col-width)
                                                          :for col :from col-start :below col-end
                                                          :do (setf (aref col-widths col) (* (aref col-widths col) ratio)))))))
           :finally
              (loop :for child :in (scene2d-box-children table)
                    :do (loop :for cell :in (scene2d-box-children child)
                              :for cell-span := (scene2d-table-cell-span cell)
                              :for col-start := 0 :then (+ col-start (scene2d-table-cell-span cell))
                              :for col-end := (+ col-start cell-span)
                              :for col-width := (loop :for col :from col-start :below col-end
                                                      :summing (aref col-widths col))
                              :do (setf (raylib:vector2-x (scene2d-table-cell-size cell)) col-width))))
     (call-next-method))))

(defstruct (scene2d-shaderable-container (:include scene2d-container))
  "A SCENE2D-CONTAINER containing a shader that is applied when rendering its child node."
  (shader nil :type raylib:shader)
  (shader-uniforms nil :type (or cobj:cobject null)))

(defmethod scene2d-draw ((container scene2d-shaderable-container) position origin scale rotation tint)
  (let ((shader (scene2d-shaderable-container-shader container)))
    (raylib:with-shader-mode shader
      (when-let ((uniforms (scene2d-shaderable-container-shader-uniforms container)))
        (update-shader-uniforms uniforms shader))
      (call-next-method))))

(defstruct (scene2d-canvas (:include scene2d-image)
                           (:constructor %make-scene2d-canvas))
  "A SCENE2D-IMAGE used for rendering anything rendered in another rendering stage, often used for implementing post-processing effects or rendering 3D scenes within a 2D scene."
  (renderer #'values :type function :read-only t))

(defvar *scene2d-canvas-renderers* nil)

(defun scene2d-canvas-render-all ()
  (loop :with renderers :of-type (vector function) := *scene2d-canvas-renderers*
        :for i :of-type non-negative-fixnum :below (length renderers)
        :do (funcall (the function (aref renderers i)))
            (setf (the function (aref renderers i)) #'values)
        :finally (setf (fill-pointer renderers) 0)))

(setf (assoc-value *game-special-bindings* '*scene2d-canvas-renderers*)
      (with-gensyms (renderers)
        `(let ((,renderers (make-array 0 :element-type 'function :adjustable t :fill-pointer 0)))
           (add-game-loop-hook #'scene2d-canvas-render-all :before t) ,renderers)))

(defun make-scene2d-canvas (&rest args &key size renderer &allow-other-keys)
  "Create a SCENE2D-CANVAS with SIZE, and use RENDERER to draw its contents. The remaining parameters ARGS are used for constructing SCENE2D-NODE."
  (declare (type raylib:vector2 size)
           (type function renderer))
  (remove-from-plistf args :size)
  (let ((width (raylib:vector2-x size))
        (height (raylib:vector2-y size)))
    (let ((render-texture (load-asset 'raylib:render-texture nil :width (ceiling width) :height (ceiling height))))
      (apply #'%make-scene2d-canvas
             :content (make-texture-region :texture (raylib:render-texture-texture render-texture)
                                           :region (raylib:make-rectangle :x 0.0 :y height :width width :height (- height)))
             :renderer (lambda ()
                         (raylib:with-texture-mode render-texture
                           (raylib:clear-background raylib:+blank+)
                           (funcall renderer)))
             args))))

(defun scene2d-canvas-render (canvas)
  (vector-push-extend (scene2d-canvas-renderer canvas) *scene2d-canvas-renderers*))

(defmethod scene2d-draw ((canvas scene2d-canvas) position origin scale rotation tint)
  (scene2d-canvas-render canvas)
  (call-next-method))

(defstruct (scene2d-tween-container (:include scene2d-container))
  "A SCENE2D-CONTAINER that includes a TWEEN-MANAGER which is destroyed together with it, eliminating the possibility of resource leaks in the global tween-manager when running an infinite-duration tween on its child nodes and forgetting to KILL it. No matter how many times the node is rendered within a single game loop iteration, its internal TWEEN-MANAGER will only be updated once."
  (manager (ute:make-tween-manager) :type ute:tween-manager)
  (speed 1.0 :type single-float))

(defmethod print-object ((container scene2d-tween-container) stream)
  (print-unreadable-object (container stream :type t :identity t)))

(defmethod scene2d-draw ((container scene2d-tween-container) position origin scale rotation tint)
  (let ((tween-manager (scene2d-tween-container-manager container)))
    (game-loop-once-only (tween-manager)
      (ute:update (* (game-loop-delta-time) (scene2d-tween-container-speed container)) tween-manager)))
  (call-next-method))

(defstruct (scene2d-rectangle (:include scene2d-layout)))

(defmethod scene2d-draw ((rectangle scene2d-rectangle) position origin scale rotation tint)
  (let ((size (scene2d-rectangle-size rectangle)))
    (clet ((rectangle (foreign-alloca '(:struct raylib:rectangle))))
      (setf (-> rectangle raylib:x) (raylib:vector2-x position)
            (-> rectangle raylib:y) (raylib:vector2-y position)
            (-> rectangle raylib:width) (* (raylib:vector2-x size) (raylib:vector2-x scale))
            (-> rectangle raylib:height) (* (raylib:vector2-y size) (raylib:vector2-y scale)))
      (raylib:%draw-rectangle-pro rectangle (& origin) rotation (& tint))))
  (call-next-method))
