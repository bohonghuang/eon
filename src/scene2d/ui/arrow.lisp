(in-package #:eon)

(define-constant +arrow-box-default-arrow-texture+
    (coerce #(137 80 78 71 13 10 26 10 0 0 0 13 73 72 68 82 0 0 0 14 0 0 0 9 8 4 0 0 0 168
              68 213 226 0 0 0 4 103 65 77 65 0 0 177 143 11 252 97 5 0 0 0 1 115 82 71 66
              1 217 201 44 127 0 0 0 32 99 72 82 77 0 0 122 38 0 0 128 132 0 0 250 0 0 0
              128 232 0 0 117 48 0 0 234 96 0 0 58 152 0 0 23 112 156 186 81 60 0 0 0 81 73
              68 65 84 24 211 101 207 177 13 0 32 8 68 209 47 113 5 87 97 4 135 119 21 134
              192 2 18 36 66 69 222 81 220 216 60 227 192 168 83 94 82 52 2 31 186 178 88
              141 229 37 160 177 116 234 44 69 150 91 60 131 12 56 249 169 201 199 39 128 5
              100 137 227 17 128 177 191 118 213 248 2 233 80 29 5 33 0 138 235 0 0 0 0 73
              69 78 68 174 66 96 130)
            '(simple-array (unsigned-byte 8) (*)))
  :test #'equalp)

(defstruct arrow-box-arrow-style
  "A structure describing the arrow style of ARROW-BOX."
  (drawable (load-asset 'raylib:texture +arrow-box-default-arrow-texture+ :format :png))
  (animation-offset '(0.0 . 4.0) :type (cons single-float single-float))
  (animation-duration 0.5 :type single-float))

(defstruct arrow-box-style
  "A structure describing the style of ARROW-BOX."
  (arrow-style (make-arrow-box-arrow-style) :type arrow-box-arrow-style))

(defstruct (arrow-box-arrow (:include scene2d-container))
  "A SCENE2D-NODE displayed as an arrow in ARROW-BOX."
  (style (make-arrow-box-arrow-style) :type arrow-box-arrow-style)
  (direction (raylib:vector2-zero) :type raylib:vector2))

(defmethod scene2d-draw ((arrow arrow-box-arrow) position origin scale rotation tint)
  (clet ((position (cthe (:pointer (:struct raylib:vector2))
                         (& (scene2d-position (arrow-box-arrow-content arrow)))))
         (direction (cthe (:pointer (:struct raylib:vector2))
                          (& (arrow-box-arrow-direction arrow)))))
    (let* ((style (arrow-box-arrow-style arrow))
           (duration (arrow-box-arrow-style-animation-duration style)))
      (destructuring-bind (lower . upper) (arrow-box-arrow-style-animation-offset style)
        (let ((distance (* (- upper lower) 2.0)))
          (raylib:%vector2-scale
           position direction
           (let ((offset (coerce (mod (/ (game-loop-time) (coerce (/ duration distance) 'double-float))
                                      (coerce distance 'double-float))
                                 'single-float)))
             (+ lower (min offset (- distance offset)))))))))
  (call-next-method))

(defstruct (arrow-box (:include scene2d-layout)
                      (:constructor %make-arrow-box))
  "A SCENE2D-NODE used to display arrows around its child node."
  (style (make-arrow-box-style) :type arrow-box-style))

(defun ensure-scene2d-node-origin-at-center (node)
  (raylib:copy-vector2
   (raylib:copy-vector2
    (raylib:vector2-scale (scene2d-size node) 0.5)
    (scene2d-origin node))
   (scene2d-position node))
  node)

(defun make-arrow-box (&rest args &key child (content child) (style (make-arrow-box-style)) (constructor #'%make-arrow-box) &allow-other-keys)
  (remove-from-plistf args :child :style :constructor)
  (apply constructor
         :style style
         :content (if (consp content)
                      content
                      (let* ((child content)
                             (style (arrow-box-style-arrow-style style))
                             (drawable (arrow-box-arrow-style-drawable style)))
                        (list child
                              (make-arrow-box-arrow
                               :style style
                               :content (ensure-scene2d-node-origin-at-center
                                         (ensure-scene2d-node drawable :rotation 0.0))
                               :direction (raylib:make-vector2 :x 0.0 :y -1.0))
                              (make-arrow-box-arrow
                               :style style
                               :content (ensure-scene2d-node-origin-at-center
                                         (ensure-scene2d-node drawable :rotation 180.0))
                               :direction (raylib:make-vector2 :x 0.0 :y 1.0))
                              (make-arrow-box-arrow
                               :style style
                               :content (ensure-scene2d-node-origin-at-center
                                         (ensure-scene2d-node drawable :rotation 270.0))
                               :direction (raylib:make-vector2 :x -1.0 :y 0.0))
                              (make-arrow-box-arrow
                               :style style
                               :content (ensure-scene2d-node-origin-at-center
                                         (ensure-scene2d-node drawable :rotation 90.0))
                               :direction (raylib:make-vector2 :x 1.0 :y 0.0)))))
         args))

(defun arrow-box-child (box)
  "Get the child of BOX."
  (first (arrow-box-content box)))

(defun (setf arrow-box-child) (child box)
  (setf (first (arrow-box-content box)) child))

(defun arrow-box-arrow (box direction)
  "Get the arrow pointing towards DIRECTION of BOX."
  (nth (ecase direction (:up 0) (:down 1) (:left 2) (:right 3))
       (rest (arrow-box-content box))))

(defmethod scene2d-layout ((box arrow-box))
  (call-next-method)
  (let* ((size (raylib:copy-vector2
                (scene2d-size (first (arrow-box-content box)))
                (arrow-box-size box)))
         (center (raylib:vector2-scale size 0.5)))
    (loop :for child :in (rest (arrow-box-content box))
          :do (raylib:copy-vector2
               (raylib:vector2-add center (raylib:vector2-multiply center (arrow-box-arrow-direction child)))
               (scene2d-position child)))))

(define-scene2d-default-construct-form arrow-box (child style))

(define-scene2d-default-construct-form arrow-box-style (arrow-style))

(define-scene2d-default-construct-form arrow-box-arrow-style (drawable animation-offset animation-duration))

(defstruct (scene2d-scroll-region-arrow-box (:include arrow-box)))

(defmethod scene2d-draw ((box scene2d-scroll-region-arrow-box) position origin scale rotation tint)
  (let* ((region (arrow-box-child box))
         (region-size (scene2d-size region))
         (child (scene2d-scroll-region-child region))
         (child-position (scene2d-position child))
         (child-size (scene2d-size child)))
    (let ((x-lower (raylib:vector2-x child-position))
          (x-upper (+ (raylib:vector2-x child-position) (raylib:vector2-x child-size)))
          (y-lower (raylib:vector2-y child-position))
          (y-upper (+ (raylib:vector2-y child-position) (raylib:vector2-y child-size))))
      (flet ((set-arrow-visibility (direction visiblep)
               (setf (raylib:color-a (scene2d-color (arrow-box-arrow box direction)))
                     (if visiblep 255 0))))
        (declare (inline set-arrow-visibility))
        (set-arrow-visibility :left (< x-lower 0.0))
        (set-arrow-visibility :right (> x-upper (raylib:vector2-x region-size)))
        (set-arrow-visibility :up (< y-lower 0.0))
        (set-arrow-visibility :down (> y-upper (raylib:vector2-y region-size))))))
  (call-next-method))

(defun scene2d-scroll-region-arrow-box (scroll-region &key (style (make-arrow-box-style)))
  "Construct an ARROW-BOX from a SCENE2D-SCROLL-REGION."
  (make-arrow-box :child scroll-region :style style :constructor #'make-scene2d-scroll-region-arrow-box))
