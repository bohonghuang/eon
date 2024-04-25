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
          :do (scene2d-draw drawable position origin scale rotation tint))))

(defun scene2d-draw-simple (drawable
                            &key
                              (position +vector2-zeros+)
                              (origin +vector2-zeros+)
                              (rotation 0.0)
                              (scale +vector2-ones+)
                              (tint raylib:+white+))
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

(defgeneric scene2d-size (node))

(defgeneric (setf scene2d-size) (value node)
  (:method (value node) (declare (ignore value node))))

(defgeneric scene2d-layout (layout)
  (:method (layout) (declare (ignore layout)))
  (:method ((list list)) (loop :for layout :in list :do (scene2d-layout layout))))

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
    node))

(defmethod scene2d-size ((list list))
  (loop :with position := (raylib:make-vector2)
        :and size := (raylib:make-vector2 :x 0.0 :y 0.0)
        :for node :in list
        :do (raylib:copy-vector2 (typecase node (scene2d-node (scene2d-node-position node)) (t +vector2-zeros+)) position)
            (raylib:%vector2-add (& position) (& position) (& (scene2d-size node)))
            (raylib:%vector2-clamp (& size) (& size) (& position) (& +vector2-max+))
        :finally (return size)))

(defstruct (scene2d-container (:include scene2d-node))
  (content nil))

(defmethod scene2d-layout ((container scene2d-container))
  (scene2d-layout (scene2d-container-content container)))

(defmethod scene2d-size ((container scene2d-container))
  (let* ((content (scene2d-container-content container))
         (size (scene2d-size content)))
    (typecase content
      (scene2d-node (raylib:vector2-multiply size (scene2d-node-scale content)))
      (t size))))

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

(defstruct scene2d-alignment
  (vertical :center :type (member :start :center :end))
  (horizontal :center :type (member :start :center :end)))

(defstruct (scene2d-layout (:include scene2d-container))
  (size (raylib:make-vector2 :x 0.0 :y 0.0) :type raylib:vector2 :read-only t))

(defmethod scene2d-size ((layout scene2d-layout))
  (scene2d-layout-size layout))

(defmethod (setf scene2d-size) (value (layout scene2d-layout))
  (raylib:copy-vector2 value (scene2d-layout-size layout)))

(defstruct (scene2d-cell (:include scene2d-layout))
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
          (child-size (scene2d-size child))
          (child-scale (etypecase child (scene2d-node (scene2d-node-scale child)) (t +vector2-ones+))))
      (macrolet ((symmetric-impl (horizontal-impl &aux (vertical-impl (copy-tree horizontal-impl)))
                   (subst-swap vertical-impl
                     (raylib:vector2-x raylib:vector2-y)
                     (scene2d-alignment-horizontal scene2d-alignment-vertical))
                   `(progn ,horizontal-impl ,vertical-impl)))
        (symmetric-impl
         (progn
           (ecase (scene2d-alignment-horizontal (scene2d-cell-alignment cell))
             (:start (setf (raylib:vector2-x (scene2d-node-position child)) 0.0))
             (:center (setf (raylib:vector2-x (scene2d-node-position child)) (/ (- (raylib:vector2-x size) (* (raylib:vector2-x child-size) (raylib:vector2-x child-scale))) 2.0)))
             (:end (setf (raylib:vector2-x (scene2d-node-position child)) (- (raylib:vector2-x size) (* (raylib:vector2-x child-size) (raylib:vector2-x child-scale))))))
           (incf (raylib:vector2-x (scene2d-node-position child)) (* (raylib:vector2-x (scene2d-node-scale child)) (raylib:vector2-x (scene2d-node-origin child))))))))))

(defstruct (scene2d-margin (:include scene2d-container)
                           (:constructor %make-scene2d-margin))
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
    (raylib:copy-vector2 (scene2d-margin-lower margin) (scene2d-node-position child))))

(defmethod scene2d-size ((margin scene2d-margin))
  (raylib:vector2-add
   (scene2d-size (scene2d-margin-content margin))
   (raylib:vector2-add
    (scene2d-margin-lower margin)
    (scene2d-margin-upper margin))))

(defstruct (scene2d-box (:include scene2d-layout))
  (orientation :vertical :type (member :vertical :horizontal)))

(defmethod scene2d-layout ((box scene2d-box))
  (macrolet ((symmetric-impl (&body vertical-impl &aux (horizontal-impl (copy-tree vertical-impl)))
               (subst-swap horizontal-impl
                 (raylib:vector2-x raylib:vector2-y)
                 (width height)
                 (x y))
               `(ecase (scene2d-box-orientation box)
                  (:vertical . ,vertical-impl)
                  (:horizontal . ,horizontal-impl))))
    (symmetric-impl
     (loop :for cell :in (scene2d-box-content box)
           :for content := (scene2d-cell-content cell)
           :do (scene2d-layout content)
           :maximize (raylib:vector2-x (scene2d-size content)) :into width :of-type single-float
           :finally (setf (raylib:vector2-x (scene2d-box-size box)) (max width 0.0)))
     (loop :with width := (raylib:vector2-x (scene2d-box-size box))
           :for cell :in (scene2d-box-content box)
           :for content := (scene2d-cell-content cell)
           :do (setf (raylib:vector2-x (scene2d-cell-size cell)) width
                     (raylib:vector2-y (scene2d-cell-size cell)) (raylib:vector2-y (scene2d-size content)))
           :do (scene2d-layout cell)
               (setf (raylib:vector2-x (scene2d-cell-position cell)) 0.0
                     (raylib:vector2-y (scene2d-cell-position cell)) height)
           :summing (raylib:vector2-y (scene2d-cell-size cell)) :into height :of-type single-float
           :finally (setf (raylib:vector2-y (scene2d-box-size box)) height)))))

(defun scene2d-box-children (box)
  (mapcar #'scene2d-cell-content (scene2d-box-content box)))

(defun scene2d-box-add-child (box child)
  (let ((cell (make-scene2d-cell :content child)))
    (nconcf (scene2d-box-content box) (list cell))
    cell))

(defun (setf scene2d-box-children) (children box)
  (setf (scene2d-box-content box) nil)
  (mapc (curry #'scene2d-box-add-child box) children))

(defun scene2d-box-remove-child (box child)
  (when-let ((cell (find child (scene2d-box-content box) :key #'scene2d-cell-content)))
    (deletef (scene2d-box-content box) cell)))

(defstruct (scene2d-nine-patch (:include scene2d-layout)))

(defmethod ensure-scene2d-node ((nine-patch n-patch) &rest args)
  (apply #'make-scene2d-nine-patch :content nine-patch args))

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

(defstruct (scene2d-window (:include scene2d-container)))

(defun scene2d-window-child (window)
  (second (scene2d-window-content window)))

(defun (setf scene2d-window-child) (child window)
  (setf (second (scene2d-window-content window)) child))

(defun scene2d-window-background (window)
  (first (scene2d-window-content window)))

(defun (setf scene2d-window-background) (background window)
  (setf (first (scene2d-window-content window)) background))

(defgeneric scene2d-window-layout (child background)
  (:method (child background)
    (setf (scene2d-size background) (scene2d-size child))))

(defmethod scene2d-layout ((window scene2d-window))
  (scene2d-layout (scene2d-window-child window))
  (scene2d-window-layout (scene2d-window-child window) (scene2d-window-background window))
  (scene2d-layout (scene2d-window-background window)))

(defmethod scene2d-size ((window scene2d-window))
  (raylib:vector2-clamp
   (scene2d-size (scene2d-window-background window))
   (scene2d-size (scene2d-window-child window))
   +vector2-max+))

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
  (alignment (make-scene2d-alignment) :type scene2d-alignment))

(defun scene2d-flow-box-children (box)
  (mapcan #'scene2d-box-children (scene2d-box-children box)))

(defun scene2d-flow-box-make-box (box)
  (let ((new-box (make-scene2d-box :orientation (ecase (scene2d-flow-box-orientation box)
                                                  (:vertical :horizontal)
                                                  (:horizontal :vertical)))))
    (scene2d-box-add-child box new-box)
    new-box))

(defun scene2d-flow-box-add-child (box child)
  (scene2d-box-add-child (or (lastcar (scene2d-box-children box))
                             (scene2d-flow-box-make-box box))
                         child))

(defmethod scene2d-layout ((box scene2d-flow-box))
  (macrolet ((symmetric-impl (&body vertical-impl &aux (horizontal-impl (copy-tree vertical-impl)))
               (subst-swap horizontal-impl
                 (raylib:vector2-x raylib:vector2-y)
                 (width height)
                 (x y)
                 (:horizontal :vertical))
               `(ecase (scene2d-box-orientation box)
                  (:vertical . ,vertical-impl)
                  (:horizontal . ,horizontal-impl))))
    (symmetric-impl
     (loop :with children := (scene2d-flow-box-children box)
           :initially (setf (scene2d-flow-box-content box) nil)
           :for new-box := (make-scene2d-box :orientation :horizontal)
           :while children
           :do (loop :for child := (first children)
                     :do (scene2d-layout child)
                     :summing (raylib:vector2-x (scene2d-size child)) :into width :of-type single-float
                     :until (and (scene2d-box-content new-box)
                                 (> width (raylib:vector2-x (scene2d-flow-box-size box))))
                     :do (setf (scene2d-cell-alignment (scene2d-box-add-child new-box (pop children)))
                               (scene2d-flow-box-alignment box))
                     :while children
                     :finally (setf (scene2d-cell-alignment (scene2d-box-add-child box new-box))
                                    (scene2d-flow-box-alignment box))))))
  (call-next-method))

(defstruct (scene2d-coordinate-truncator (:include scene2d-container)))

(defmethod scene2d-draw ((truncator scene2d-coordinate-truncator) position origin scale rotation tint)
  (declare (ignore origin rotation scale tint))
  (setf (raylib:vector2-x position) (ftruncate (raylib:vector2-x position))
        (raylib:vector2-y position) (ftruncate (raylib:vector2-y position)))
  (call-next-method))

(defstruct scene2d-label-style
  (text-style (make-text-style) :type text-style)
  (color (raylib:make-color :r 74 :g 73 :b 74 :a 255) :type raylib:color)
  (shadow (raylib:make-color :r 214 :g 215 :b 206 :a 255) :type (or raylib:color null))
  (outline nil :type (or raylib:color null)))

(defstruct (scene2d-label (:include scene2d-coordinate-truncator))
  (style (make-scene2d-label-style) :type scene2d-label-style))

(defmethod ensure-scene2d-node ((text text) &rest args)
  (apply #'make-scene2d-label :content text args))

(defun scene2d-label-string (label)
  (if-let ((text (scene2d-label-content label)))
    (text-string text) ""))

(defun (setf scene2d-label-string) (text label)
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

(defstruct (scene2d-scissor (:include scene2d-layout)))

(defmethod scene2d-draw ((scissor scene2d-scissor) position origin scale rotation tint)
  (raylib:with-scissor-mode ((floor (raylib:vector2-x position))
                             (floor (raylib:vector2-y position))
                             (floor (raylib:vector2-x (scene2d-scissor-size scissor)))
                             (floor (raylib:vector2-y (scene2d-scissor-size scissor))))
    (call-next-method)))

(defstruct (scene2d-image (:include scene2d-container)))

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
                                     (interval 0.0 interval-p)
                                     (restore-frame-p nil))
  (declare (type single-float duration interval))
  (tween-iteration-in-place
      ((scene2d-image-content image) frames)
      :duration (if interval-p (* interval (length frames)) duration)
      :repeat (:count (if repeat repeat 0))
      :ease #'ute:linear-inout
      :restore-place-p restore-frame-p))

(defstruct (scene2d-group (:include scene2d-container)
                          (:constructor %make-scene2d-group)))

(defun make-scene2d-group (&rest args)
  (apply #'%make-scene2d-group :content (make-scene2d-container) args))

(defun scene2d-group-children (group)
  (scene2d-container-content (scene2d-group-content group)))

(defun scene2d-group-add-child (group child)
  (nconcf (scene2d-container-content (scene2d-group-content group)) (list child)))

(defun scene2d-group-remove-child (group child &key (from-end nil) (test #'eq) (key #'identity))
  (deletef (scene2d-container-content (scene2d-group-content group)) child :from-end from-end :test test :key key))

(defmethod scene2d-size ((group scene2d-group))
  (loop :with result := (raylib:make-vector2 :x 0.0 :y 0.0)
        :for child :in (scene2d-group-children group)
        :do (raylib:%vector2-clamp (& result) (& result) (& +vector2-min+) (& (scene2d-node-position child)))
        :finally
           (raylib:%vector2-subtract (& result) (& (scene2d-size (scene2d-group-content group))) (& result))
           (return result)))

(defmethod scene2d-layout ((group scene2d-group))
  (call-next-method)
  (raylib:%vector2-subtract
   (& (scene2d-container-position (scene2d-group-content group)))
   (& (scene2d-size group))
   (& (scene2d-size (scene2d-group-content group)))))

(defstruct scene2d-dimensions
  (rows 2 :type fixnum)
  (columns 1 :type fixnum))

(defstruct (scene2d-max-cell (:include scene2d-cell)))

(defmethod scene2d-size ((cell scene2d-max-cell))
  (raylib:vector2-clamp
   (scene2d-size (scene2d-max-cell-content cell))
   (scene2d-max-cell-size cell)
   +vector2-max+))

(defstruct (scene2d-table-cell (:include scene2d-max-cell))
  (span 1 :type positive-fixnum))

(defstruct (scene2d-table (:include scene2d-box)))

(defun scene2d-table-newline (table)
  (let* ((box (make-scene2d-box :orientation (ecase (scene2d-table-orientation table)
                                               (:vertical :horizontal)
                                               (:horizontal :vertical))))
         (cell (scene2d-box-add-child table box)))
    (setf (scene2d-cell-alignment cell) (make-scene2d-alignment :vertical :start :horizontal :start))
    cell))

(defun scene2d-table-add-child (table child)
  (let ((box (lastcar (scene2d-box-children table)))
        (cell (make-scene2d-table-cell :content child)))
    (scene2d-box-add-child box cell) cell))

(defun scene2d-table-children (table)
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
  (shader nil :type raylib:shader))

(defmethod scene2d-draw ((container scene2d-shaderable-container) position origin scale rotation tint)
  (raylib:with-shader-mode (scene2d-shaderable-container-shader container)
    (call-next-method)))

(defstruct (scene2d-canvas (:include scene2d-image)
                           (:constructor %make-scene2d-canvas))
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
  (manager (ute:make-tween-manager) :type ute:tween-manager)
  (speed 1.0 :type single-float))

(defmethod print-object ((container scene2d-tween-container) stream)
  (print-unreadable-object (container stream :type t :identity t)))

(defmethod scene2d-draw ((container scene2d-tween-container) position origin scale rotation tint)
  (let ((tween-manager (scene2d-tween-container-manager container)))
    (game-loop-once-only (tween-manager)
      (ute:update (* (game-loop-delta-time) (scene2d-tween-container-speed container)) tween-manager)))
  (call-next-method))
