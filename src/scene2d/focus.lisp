(in-package #:eon)

(defstruct (scene2d-focusable (:include scene2d-container))
  "A SCENE2D-CONTAINER that contains a FOCAL-POINTS (world coordinate of its child node) used in SCENE2D-FOCUS-MANAGER."
  (focal-bounds (cons (raylib:vector2-zero) (raylib:vector2-zero)) :type (cons raylib:vector2 raylib:vector2)))

(declaim (inline scene2d-focusable-focal-bounds-value))
(defun scene2d-focusable-focal-bounds-value (focusable direction)
  (destructuring-bind (lower . upper) (scene2d-focusable-focal-bounds focusable)
    (ecase direction
      (:up (raylib:vector2-y lower))
      (:down (raylib:vector2-y upper))
      (:left (raylib:vector2-x lower))
      (:right (raylib:vector2-x upper)))))

(cobj:define-global-cobject +scene2d-focusable-up+ (raylib:make-vector2 :x 0.0 :y -1.0))
(cobj:define-global-cobject +scene2d-focusable-down+ (raylib:make-vector2 :x 0.0 :y 1.0))
(cobj:define-global-cobject +scene2d-focusable-left+ (raylib:make-vector2 :x -1.0 :y 0.0))
(cobj:define-global-cobject +scene2d-focusable-right+ (raylib:make-vector2 :x 1.0 :y 0.0))
(cobj:define-global-cobject +scene2d-focusable-up-left+ (raylib:vector2-add +scene2d-focusable-up+ +scene2d-focusable-left+))

(defun scene2d-focusable-focal-point (focusable &optional direction)
  (let ((offset (ecase direction
                  (:up +scene2d-focusable-up+)
                  (:down +scene2d-focusable-down+)
                  (:left +scene2d-focusable-left+)
                  (:right +scene2d-focusable-right+)
                  ((t) +vector2-zeros+)
                  ((nil) +scene2d-focusable-up-left+))))
    (destructuring-bind (lower . upper) (scene2d-focusable-focal-bounds focusable)
      (clet ((point (raylib:vector2-scale (raylib:vector2-add lower upper) 0.5))
             (size (raylib:vector2-subtract upper lower)))
        (declare (dynamic-extent size))
        (raylib:%vector2-scale (& size) (& size) 0.5)
        (raylib:%vector2-multiply (& size) (& size) (& offset))
        (raylib:%vector2-add (& point) (& point) (& size))
        point))))

(declaim (ftype (function (scene2d-focusable single-float single-float)) scene2d-focusable-update-upper-focal-bounds))
(defun scene2d-focusable-update-upper-focal-bounds (focusable size-x size-y)
  (destructuring-bind (lower . upper) (scene2d-focusable-focal-bounds focusable)
    (setf (raylib:vector2-x upper) (+ (raylib:vector2-x lower) size-x)
          (raylib:vector2-y upper) (+ (raylib:vector2-y lower) size-y))))

(defmethod scene2d-layout ((focusable scene2d-focusable))
  (call-next-method)
  (let ((size (rectangle-size (scene2d-bound focusable))))
    (scene2d-focusable-update-upper-focal-bounds focusable (raylib:vector2-x size) (raylib:vector2-y size))))

(defmethod scene2d-draw ((focusable scene2d-focusable) position origin scale rotation tint)
  (destructuring-bind (lower . upper) (scene2d-focusable-focal-bounds focusable)
    (assert (<= (raylib:vector2-x lower) (raylib:vector2-x upper)))
    (assert (<= (raylib:vector2-y lower) (raylib:vector2-y upper)))
    (let ((size-x (- (raylib:vector2-x upper) (raylib:vector2-x lower)))
          (size-y (- (raylib:vector2-y upper) (raylib:vector2-y lower))))
      (raylib:%vector2-subtract (& lower) (& position) (& origin))
      (scene2d-focusable-update-upper-focal-bounds focusable size-x size-y))
    (call-next-method)))

(defparameter *scene2d-focus-manager-distance-ratio* 0.25)

(defstruct scene2d-focus-manager
  "A structure that contains a list of SCENE2D-FOCUSABLEs."
  (focusables nil :type list)
  (distance-ratio *scene2d-focus-manager-distance-ratio* :type single-float))

(defun scene2d-focus-manager-focused (manager)
  "Get the currently focused SCENE2D-FOCUSABLE."
  (first (scene2d-focus-manager-focusables manager)))

(defun (setf scene2d-focus-manager-focused) (value manager)
  "Set the currently focused SCENE2D-FOCUSABLE."
  (rotatef
   (nth 0 (scene2d-focus-manager-focusables manager))
   (nth (position value (scene2d-focus-manager-focusables manager))
        (scene2d-focus-manager-focusables manager)))
  value)

(defun scene2d-focus-manager-handle-input (manager button)
  "Make the MANAGER change the currently focused SCENE2D-FOCUSABLE based on the BUTTON (which can be :LEFT, :RIGHT, :UP, or :DOWN)."
  (with-accessors ((focusables scene2d-focus-manager-focusables))
      manager
    (let ((distance-ratio (scene2d-focus-manager-distance-ratio manager))
          (focused (first focusables)))
      (macrolet ((-+ (a b) `(- ,b ,a))
                 (+- (a b) `(- ,a ,b))
                 (symmetric-impl (&body right-impl &aux
                                                     (left-impl (copy-tree right-impl))
                                                     (up-impl (copy-tree right-impl))
                                                     (down-impl (copy-tree right-impl)))
                   (subst-swap left-impl
                     (:minimize :maximize)
                     (:right :left)
                     (+- -+))
                   (subst-swap up-impl
                     (:minimize :maximize)
                     (:right :up)
                     (:left :down)
                     (+- -+)
                     (raylib:vector2-x raylib:vector2-y))
                   (subst-swap down-impl
                     (:right :down)
                     (:left :up)
                     (raylib:vector2-x raylib:vector2-y))
                   `(ecase button
                      (:up . ,up-impl)
                      (:down . ,down-impl)
                      (:left . ,left-impl)
                      (:right . ,right-impl))))
        (symmetric-impl
         (multiple-value-bind (lower-bound upper-bound)
             (loop :for focusable :in focusables
                   :minimize (scene2d-focusable-focal-bounds-value focusable :left) :into lower-bound :of-type single-float
                   :maximize (scene2d-focusable-focal-bounds-value focusable :right) :into upper-bound :of-type single-float
                   :finally (return (values lower-bound upper-bound)))
           (let ((focused-bound-value (scene2d-focusable-focal-bounds-value focused :right))
                 (bound (+- upper-bound lower-bound)))
             (flet ((directional-distance (focusable &aux (focal-bounds-value (scene2d-focusable-focal-bounds-value focusable :left)))
                      (let ((distance (+- focal-bounds-value focused-bound-value)))
                        (if (minusp distance) (+ (+- focal-bounds-value lower-bound) (+- upper-bound focused-bound-value) bound) distance)))
                    (non-directional-distance (focusable)
                      (abs (/ (+ (- (scene2d-focusable-focal-bounds-value focusable :up) (scene2d-focusable-focal-bounds-value focused :up))
                                 (- (scene2d-focusable-focal-bounds-value focusable :down) (scene2d-focusable-focal-bounds-value focused :down)))
                              2.0))))
               (flet ((distance (focusable)
                        (if (and (= (scene2d-focusable-focal-bounds-value focused :left)
                                    (scene2d-focusable-focal-bounds-value focusable :left))
                                 (= (scene2d-focusable-focal-bounds-value focused :right)
                                    (scene2d-focusable-focal-bounds-value focusable :right)))
                            most-positive-single-float
                            (lerp distance-ratio (non-directional-distance focusable) (directional-distance focusable)))))
                 (setf focusables (sort focusables #'< :key #'distance)))))))))))
