(in-package #:eon)

(cobj:define-global-cobject +vector3-unit-x+ (raylib:make-vector3 :x 1.0 :y 0.0 :z 0.0))
(cobj:define-global-cobject +vector3-unit-y+ (raylib:make-vector3 :x 0.0 :y 1.0 :z 0.0))
(cobj:define-global-cobject +vector3-unit-z+ (raylib:make-vector3 :x 0.0 :y 0.0 :z 1.0))
(cobj:define-global-cobject +vector3-zeros+ (raylib:make-vector3 :x 0.0 :y 0.0 :z 0.0))
(cobj:define-global-cobject +vector3-ones+ (raylib:make-vector3 :x 1.0 :y 1.0 :z 1.0))
(cobj:define-global-cobject +vector3-max+ (raylib:make-vector3 :x most-positive-single-float :y most-positive-single-float :z most-positive-single-float))
(cobj:define-global-cobject +vector3-min+ (raylib:make-vector3 :x most-negative-single-float :y most-negative-single-float :z most-negative-single-float))

(cobj:define-global-cobject +quaternion-identity+ (raylib:make-quaternion :x 0.0 :y 0.0 :z 0.0 :w 1.0))

(defgeneric scene3d-draw (drawable position origin scale rotation tint)
  (:method ((list list) position origin scale rotation tint)
    (loop :for drawable :in list
          :do (scene3d-draw drawable position origin scale rotation tint))))

(defun scene3d-draw-simple (drawable
                            &key
                              (position +vector3-zeros+)
                              (origin +vector3-zeros+)
                              (scale +vector3-ones+)
                              (rotation +quaternion-identity+)
                              (tint raylib:+white+))
  (scene3d-draw drawable position origin scale rotation tint))

(defgeneric scene3d-bound (object))

(defgeneric scene3d-layout (object)
  (:method (layout) (declare (ignore layout)))
  (:method ((list list)) (loop :for layout :in list :do (scene3d-layout layout))))

(defstruct (scene3d-node (:constructor nil))
  (position (raylib:make-vector3 :x 0.0 :y 0.0 :z 0.0) :type raylib:vector3 :read-only t)
  (origin (raylib:make-vector3 :x 0.0 :y 0.0 :z 0.0) :type raylib:vector3 :read-only t)
  (scale (raylib:make-vector3 :x 1.0 :y 1.0 :z 1.0) :type raylib:vector3 :read-only t)
  (rotation (raylib:make-quaternion :x 0.0 :y 0.0 :z 0.0 :w 1.0) :type raylib:vector4 :read-only t)
  (color (raylib:make-color :r 255 :g 255 :b 255 :a 255) :type raylib:color :read-only t))

(setf (fdefinition 'scene3d-position) (fdefinition 'scene3d-node-position)
      (fdefinition 'scene3d-scale) (fdefinition 'scene3d-node-scale)
      (fdefinition 'scene3d-rotation) (fdefinition 'scene3d-node-rotation)
      (fdefinition 'scene3d-color) (fdefinition 'scene3d-node-color))

(defgeneric ensure-scene3d-node (object &rest args)
  (:method ((node scene3d-node) &key
                                  (position (scene3d-node-position node))
                                  (scale (scene3d-node-scale node))
                                  (color (scene3d-node-color node))
                                  (rotation (scene3d-node-rotation node)))
    (raylib:copy-vector3 position (scene3d-node-position node))
    (raylib:copy-vector3 scale (scene3d-node-scale node))
    (raylib:copy-quaternion rotation (scene3d-node-rotation node))
    (raylib:copy-color color (scene3d-node-color node))
    node))

(defmethod scene3d-bound ((list list))
  (loop :with position := (raylib:make-vector3)
        :and bound := (raylib:make-bounding-box :min +vector3-max+ :max +vector3-min+)
        :for node :in list
        :for node-bound := (scene3d-bound node)
        :do (raylib:copy-vector3 (typecase node (scene3d-node (scene3d-node-position node)) (t +vector3-zeros+)) position)
            (raylib:%vector3-add (& position) (& position) (& (raylib:bounding-box-min node-bound)))
            (raylib:%vector3-clamp (& (raylib:bounding-box-min bound)) (& (raylib:bounding-box-min bound)) (& +vector3-min+) (& position))
            (raylib:copy-vector3 (typecase node (scene3d-node (scene3d-node-position node)) (t +vector3-zeros+)) position)
            (raylib:%vector3-add (& position) (& position) (& (raylib:bounding-box-max node-bound)))
            (raylib:%vector3-clamp (& (raylib:bounding-box-max bound)) (& (raylib:bounding-box-max bound)) (& position) (& +vector3-max+))
        :finally (return bound)))

(defstruct (scene3d-container (:include scene3d-node))
  (content nil))

(defmacro with-scene3d-container-transform ((container (position origin scale rotation tint)) &body body)
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
                 original-color
                 original-rotation)
    `(let ((,target-position (& (scene3d-container-position ,container)))
           (,target-origin (& (scene3d-container-origin ,container)))
           (,target-scale (& (scene3d-container-scale ,container)))
           (,target-color (& (scene3d-container-color ,container)))
           (,target-rotation (& (scene3d-container-rotation ,container)))
           (,offset-position (& ,position))
           (,offset-origin (& ,origin))
           (,offset-scale (& ,scale))
           (,offset-color (& ,tint))
           (,offset-rotation (& ,rotation)))
       (clocally (declare (ctype (:pointer (:struct raylib:vector3))
                                 ,target-position ,target-origin ,target-scale
                                 ,offset-position ,offset-origin ,offset-scale)
                          (ctype (:pointer (:struct raylib:vector4))
                                 ,target-rotation ,offset-rotation)
                          (ctype (:pointer (:struct raylib:color))
                                 ,target-color ,offset-color))
         (clet ((,original-position (foreign-alloca '(:struct raylib:vector3)))
                (,original-origin (foreign-alloca '(:struct raylib:vector3)))
                (,original-rotation (foreign-alloca '(:struct raylib:vector4)))
                (,original-scale (foreign-alloca '(:struct raylib:vector3)))
                (,original-color (foreign-alloca '(:struct raylib:color))))
           (csetf ([] ,original-position) ([] ,target-position)
                  ([] ,original-origin) ([] ,target-origin)
                  ([] ,original-rotation) ([] ,target-rotation)
                  ([] ,original-scale) ([] ,target-scale)
                  ([] ,original-color) ([] ,target-color))
           (raylib:%vector3-add ,target-position ,target-position ,offset-position)
           (raylib:%vector3-add ,target-origin ,target-origin ,offset-origin)
           (raylib:%vector3-multiply ,target-scale ,target-scale ,offset-scale)
           (raylib:%quaternion-multiply ,target-rotation ,target-rotation ,offset-rotation)
           (raylib:%color-tint ,target-color ,target-color ,offset-color)
           (let ((,position (scene3d-container-position ,container))
                 (,origin (scene3d-container-origin ,container))
                 (,rotation (scene3d-container-rotation ,container))
                 (,scale (scene3d-container-scale ,container))
                 (,tint (scene3d-container-color ,container)))
             ,@body)
           (csetf ([] ,target-position) ([] ,original-position)
                  ([] ,target-origin) ([] ,original-origin)
                  ([] ,target-rotation) ([] ,original-rotation)
                  ([] ,target-scale) ([] ,original-scale)
                  ([] ,target-color) ([] ,original-color)))))))

(defmethod scene3d-draw ((container scene3d-container) position origin scale rotation tint)
  (scene3d-draw (scene3d-container-content container) position origin scale rotation tint))

(defmethod scene3d-draw :around ((container scene3d-container) position origin scale rotation tint)
  (with-scene3d-container-transform (container (position origin scale rotation tint))
    (call-next-method container position origin scale rotation tint)))

(defmethod scene3d-bound ((container scene3d-container))
  (let* ((content (scene3d-container-content container))
         (bound (scene3d-bound content)))
    (typecase content
      (scene3d-node
       (let* ((scale (scene3d-node-scale content))
              (bound (raylib:copy-bounding-box bound))
              (bound-min (raylib:bounding-box-min bound))
              (bound-max (raylib:bounding-box-max bound)))
         (raylib:%vector3-multiply (& bound-min) (& bound-min) (& scale))
         (raylib:%vector3-multiply (& bound-max) (& bound-max) (& scale))
         bound))
      (t bound))))

(defmethod scene3d-layout ((container scene3d-container))
  (scene3d-layout (scene3d-container-content container)))

(defmethod scene3d-draw ((model raylib:model) position origin scale rotation tint)
  (assert (raylib:vector3-equal origin +vector3-zeros+))
  (clet ((axis (foreign-alloca '(:struct raylib:vector3)))
         (angle (foreign-alloca :float)))
    (raylib:%quaternion-to-axis-angle (& rotation) axis angle)
    (raylib:%draw-model-ex (& model) (& position) axis (radian-degree ([] angle)) (& scale) (& tint))))

(declaim (inline %model-bound))
(defun %model-bound (result model &aux (vector3-min (& +vector3-min+)) (vector3-max (& +vector3-max+)))
  (clet ((bounding-box (foreign-alloca '(:struct raylib:bounding-box))))
    (declare (ctype (:pointer (:struct raylib:model)) model)
             (ctype (:pointer (:struct raylib:bounding-box)) result)
             (ctype (:pointer (:struct raylib:vector3)) vector3-min vector3-max))
    (csetf (-> result raylib:min) ([] vector3-max)
           (-> result raylib:max) ([] vector3-min))
    (loop :for i :below (-> model raylib:mesh-count)
          :do (raylib:%get-mesh-bounding-box bounding-box ([] (-> model raylib:meshes) i))
              (raylib:%vector3-clamp (& (-> result raylib:min)) (& (-> result raylib:min)) vector3-min (& (-> bounding-box raylib:min)))
              (raylib:%vector3-clamp (& (-> result raylib:max)) (& (-> result raylib:max)) (& (-> bounding-box raylib:max)) vector3-max))))

(defmethod scene3d-bound ((model raylib:model))
  (let ((bounding-box (raylib:make-bounding-box)))
    (%model-bound (& bounding-box) (& model))
    bounding-box))

(defmethod ensure-scene3d-node ((object raylib:model) &rest args)
  (apply #'make-scene3d-container :content object args))

(defvar *scene3d-camera* nil)

(defstruct (scene3d-billboard (:include scene3d-container))
  (up (raylib:make-vector3 :x 0.0 :y 1.0 :z 0.0) :type raylib:vector3 :read-only t))

(defmethod scene3d-draw ((billboard scene3d-billboard) position origin scale rotation tint)
  (with-accessors ((up scene3d-billboard-up)) billboard
    (let ((texture-region (scene3d-billboard-content billboard))
          (rotation (clet ((euler (foreign-alloca '(:struct raylib:vector3))))
                      (raylib:%quaternion-to-euler euler (& rotation))
                      (radian-degree (-> euler raylib:z))))
          (original-scale-x (raylib:vector3-x scale))
          (original-scale-y (raylib:vector3-y scale))
          (original-position-y (raylib:vector3-y position))
          (original-origin-x (raylib:vector3-x origin))
          (original-origin-y (raylib:vector3-y origin)))
      (setf (raylib:vector3-y scale) (* (texture-region-height texture-region) (raylib:vector3-y scale))
            (raylib:vector3-x scale) (* (texture-region-height texture-region) (raylib:vector3-x scale))
            (raylib:vector3-y position) (- (raylib:vector3-y position)
                                           (* (raylib:vector3-y scale)
                                              (- (/ 2.0) (/ (raylib:vector3-y origin)
                                                            (texture-region-height texture-region)))))
            (raylib:vector3-x origin) (- (/ (* (raylib:vector3-x origin) 2.0) (texture-region-width texture-region)) 1.0)
            (raylib:vector3-y origin) (- 1.0 (/ (* (raylib:vector3-y origin) 2.0) (texture-region-height texture-region))))
      (raylib:%draw-billboard-pro (& *scene3d-camera*) (& (texture-region-texture texture-region))
                                  (& (texture-region-region texture-region)) (& position)
                                  (& (scene3d-billboard-up billboard)) (& scale)
                                  (& origin) rotation (& tint))
      (setf (raylib:vector3-x scale) original-scale-x
            (raylib:vector3-y scale) original-scale-y
            (raylib:vector3-y position) original-position-y
            (raylib:vector3-x origin) original-origin-x
            (raylib:vector3-y origin) original-origin-y))))

(defmethod ensure-scene3d-node ((region texture-region)
                                &rest args
                                &key (origin (raylib:make-vector3 :x (/ (texture-region-width region) 2.0)
                                                                  :y (/ (texture-region-height region) 2.0)
                                                                  :z 0.0))
                                &allow-other-keys)
  (remove-from-plistf args :origin)
  (apply #'make-scene3d-billboard :content region :origin origin args))

(defmethod ensure-scene3d-node ((texture raylib:texture) &rest args)
  (apply #'ensure-scene3d-node (make-texture-region :texture texture) args))

(defmethod ensure-scene3d-node ((image raylib:image) &rest args)
  (apply #'ensure-scene3d-node (load-asset 'raylib:texture image) args))

(defmethod ensure-scene3d-node ((texture raylib:render-texture) &rest args)
  (apply #'ensure-scene3d-node (make-texture-region :texture (raylib:render-texture-texture texture)) args))

(defmethod scene3d-bound ((texture-region texture-region))
  (let ((bounding-box (raylib:make-bounding-box :min +vector3-zeros+ :max +vector3-zeros+)))
    (setf (raylib:vector3-y (raylib:bounding-box-max bounding-box)) (texture-region-height texture-region))
    bounding-box))

(defun scene3d-billboard-tween-frames (billboard frames
                                       &key
                                         (duration 1.0)
                                         (repeat nil)
                                         (interval 0.0 interval-p)
                                         (restore-frame-p nil))
  (declare (type single-float duration interval))
  (tween-iteration-in-place
      ((scene3d-billboard-content billboard) frames)
      :duration (if interval-p (* interval (length frames)) duration)
      :repeat (:count (if repeat repeat 0))
      :ease #'ute:linear-inout
      :restore-place-p restore-frame-p))

(defstruct (scene3d-layout (:include scene3d-container))
  (bound (raylib:make-bounding-box :min +vector3-zeros+ :max +vector3-zeros+)))

(defmethod scene3d-bound ((layout scene3d-layout))
  (scene3d-layout-bound layout))

(defstruct scene3d-alignment
  (x :center :type (member :start :center :end))
  (y :center :type (member :start :center :end))
  (z :center :type (member :start :center :end)))

(defstruct (scene3d-cell (:include scene3d-layout))
  (alignment (make-scene3d-alignment) :type scene3d-alignment))

(defmethod scene3d-layout ((cell scene3d-cell))
  (call-next-method)
  (when-let ((child (scene3d-cell-content cell)))
    (let* ((align (scene3d-cell-alignment cell))
           (bound (scene3d-bound cell))
           (bound-min (raylib:bounding-box-min bound))
           (bound-max (raylib:bounding-box-max bound))
           (child-bound (scene3d-bound child))
           (child-bound-min (raylib:bounding-box-min child-bound))
           (child-bound-max (raylib:bounding-box-max child-bound)))
      (macrolet ((symmetric-impl (x-impl &aux (y-impl (copy-tree x-impl)) (z-impl (copy-tree x-impl)))
                   (subst-swap y-impl
                     (raylib:vector3-x raylib:vector3-y)
                     (scene3d-alignment-x scene3d-alignment-y))
                   (subst-swap z-impl
                     (raylib:vector3-x raylib:vector3-z)
                     (scene3d-alignment-x scene3d-alignment-z))
                   `(progn ,x-impl ,y-impl ,z-impl)))
        (symmetric-impl
         (ecase (scene3d-alignment-x align)
           (:start (setf (raylib:vector3-x (scene3d-node-position child)) (- (raylib:vector3-x bound-min) (raylib:vector3-x child-bound-min))))
           (:center (setf (raylib:vector3-x (scene3d-node-position child)) (/ (+ (- (raylib:vector3-x bound-min) (raylib:vector3-x child-bound-min))
                                                                                 (- (raylib:vector3-x bound-max) (raylib:vector3-x child-bound-max)))
                                                                              2.0)))
           (:end (setf (raylib:vector3-x (scene3d-node-position child)) (- (raylib:vector3-x bound-max) (raylib:vector3-x child-bound-max))))))))))

(defstruct (scene3d-margin (:include scene3d-container)
                           (:constructor %make-scene3d-margin))
  (lower (raylib:make-vector3 :x 0.0 :y 0.0 :z 0.0))
  (upper (raylib:make-vector3 :x 0.0 :y 0.0 :z 0.0)))

(defun make-scene3d-margin (&rest args &key top bottom left right front back &allow-other-keys)
  (remove-from-plistf args :top :bottom :left :right :front :back)
  (let ((margin (apply #'%make-scene3d-margin args)))
    (with-accessors ((lower scene3d-margin-lower)
                     (upper scene3d-margin-upper))
        margin
      (setf (raylib:vector3-x lower) (or left 0.0)
            (raylib:vector3-y lower) (or top 0.0)
            (raylib:vector3-z lower) (or back 0.0)
            (raylib:vector3-x upper) (or right 0.0)
            (raylib:vector3-y upper) (or bottom 0.0)
            (raylib:vector3-z upper) (or front 0.0))
      margin)))

(defmethod scene3d-bound ((margin scene3d-margin))
  (let* ((bound (raylib:copy-bounding-box (scene3d-bound (scene3d-margin-content margin))))
         (bound-min (raylib:bounding-box-min bound))
         (bound-max (raylib:bounding-box-max bound)))
    (raylib:%vector3-subtract (& bound-min) (& bound-min) (& (scene3d-margin-lower margin)))
    (raylib:%vector3-add (& bound-max) (& bound-max) (& (scene3d-margin-upper margin)))
    bound))

(defstruct (scene3d-canvas (:include scene3d-billboard)
                           (:constructor %make-scene3d-canvas))
  (renderer #'values :type function :read-only t))

(defvar *scene3d-canvas-renderers* nil)

(defun scene3d-canvas-render-all ()
  (loop :with renderers :of-type (vector function) := *scene3d-canvas-renderers*
        :for i :of-type non-negative-fixnum :below (length renderers)
        :do (funcall (the function (aref renderers i)))
            (setf (the function (aref renderers i)) #'values)
        :finally (setf (fill-pointer renderers) 0)))

(setf (assoc-value *game-special-bindings* '*scene3d-canvas-renderers*)
      (with-gensyms (renderers)
        `(let ((,renderers (make-array 0 :element-type 'function :adjustable t :fill-pointer 0)))
           (add-game-loop-hook #'scene3d-canvas-render-all :before t) ,renderers)))

(defun make-scene3d-canvas (&rest args &key size renderer &allow-other-keys)
  (declare (type raylib:vector2 size)
           (type function renderer))
  (remove-from-plistf args :size)
  (let ((width (raylib:vector2-x size))
        (height (raylib:vector2-y size)))
    (let ((render-texture (load-asset 'raylib:render-texture nil :width (ceiling width) :height (ceiling height))))
      (apply #'%make-scene3d-canvas
             :content (make-texture-region :texture (raylib:render-texture-texture render-texture)
                                           :region (raylib:make-rectangle :x 0.0 :y height :width width :height (- height)))
             :renderer (lambda ()
                         (raylib:with-texture-mode render-texture
                           (raylib:clear-background raylib:+blank+)
                           (funcall renderer)))
             args))))

(defun scene3d-canvas-render (canvas)
  (vector-push-extend (scene3d-canvas-renderer canvas) *scene3d-canvas-renderers*))

(defmethod scene3d-draw ((canvas scene3d-canvas) position origin scale rotation tint)
  (scene3d-canvas-render canvas)
  (call-next-method))

(defmethod ensure-scene3d-node ((node scene2d-node) &rest args)
  (let ((size (scene2d-size node)))
    (apply #'make-scene3d-canvas :size size
                                 :renderer (curry #'scene2d-draw-simple node)
                                 :origin (raylib:make-vector3 :x (/ (raylib:vector2-x size) 2.0)
                                                              :y (/ (raylib:vector2-y size) 2.0)
                                                              :z 0.0)
                                 args)))

(defstruct (scene3d-shaderable-container (:include scene3d-container))
  (shader nil :type raylib:shader))

(defmethod scene3d-draw ((container scene3d-shaderable-container) position origin scale rotation tint)
  (raylib:with-shader-mode (scene3d-shaderable-container-shader container)
    (call-next-method)))

(defun rlgl-apply-scene3d-draw-arguments (position origin scale rotation tint)
  (rlgl:translatef (raylib:vector3-x position) (raylib:vector3-y position) (raylib:vector3-z position))
  (rlgl:scalef (raylib:vector3-x scale) (raylib:vector3-y scale) (raylib:vector3-z scale))
  (clet* ((axis+angle (foreign-alloca '(:struct raylib:vector4)))
          (axis (cthe (:pointer (:struct raylib:vector4)) (& axis+angle)))
          (angle (cthe (:pointer :float) (& (-> axis+angle raylib:w)))))
    (raylib:%quaternion-to-axis-angle (& rotation) axis angle)
    (rlgl:rotatef (radian-degree ([] angle)) (-> axis raylib:x) (-> axis raylib:y) (-> axis raylib:z)))
  (rlgl:translatef (- (raylib:vector3-x origin)) (- (raylib:vector3-y origin)) (- (raylib:vector3-z origin)))
  (rlgl:color4ub (raylib:color-r tint) (raylib:color-g tint) (raylib:color-b tint) (raylib:color-a tint)))
