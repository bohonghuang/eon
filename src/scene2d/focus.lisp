(in-package #:eon)

(defstruct (scene2d-focusable (:include scene2d-container))
  "A SCENE2D-CONTAINER that contains a FOCAL-POINTS (world coordinate of its child node) used in SCENE2D-FOCUS-MANAGER."
  (focal-points (loop :repeat 4 :collect (raylib:vector2-zero)) :type list))

(defun scene2d-focusable-focal-point (focusable &optional direction)
  (nth (ecase direction
         (:up 0) (:down 1) (:left 2) (:right 3)
         ((t) (return-from scene2d-focusable-focal-point
                (raylib:vector2-scale (reduce #'raylib:vector2-add (scene2d-focusable-focal-points focusable)) (/ 4.0))))
         ((nil) (return-from scene2d-focusable-focal-point
                  (raylib:make-vector2
                   :x (raylib:vector2-x (scene2d-focusable-focal-point focusable :left))
                   :y (raylib:vector2-y (scene2d-focusable-focal-point focusable :up))))))
       (scene2d-focusable-focal-points focusable)))

(declaim (ftype (function (scene2d-focusable single-float single-float)) scene2d-focusable-update-upper-focal-points))
(defun scene2d-focusable-update-upper-focal-points (focusable size-x size-y)
  (destructuring-bind (up down left right) (scene2d-focusable-focal-points focusable)
    (setf (raylib:vector2-y right) (raylib:vector2-y left)
          (raylib:vector2-x right) (+ (raylib:vector2-x left) size-x))
    (setf (raylib:vector2-x down) (raylib:vector2-x up)
          (raylib:vector2-y down) (+ (raylib:vector2-y up) size-y))))

(defmethod scene2d-layout ((focusable scene2d-focusable))
  (call-next-method)
  (let ((size (scene2d-size (scene2d-focusable-content focusable))))
    (scene2d-focusable-update-upper-focal-points focusable (raylib:vector2-x size) (raylib:vector2-y size))))

(defmethod scene2d-draw ((focusable scene2d-focusable) position origin scale rotation tint)
  (destructuring-bind (up down left right) (scene2d-focusable-focal-points focusable)
    (assert (= (raylib:vector2-x up) (raylib:vector2-x down)))
    (assert (<= (raylib:vector2-y up) (raylib:vector2-y down)))
    (assert (= (raylib:vector2-y left) (raylib:vector2-y right)))
    (assert (<= (raylib:vector2-x left) (raylib:vector2-x right)))
    (let ((size-x (- (raylib:vector2-x right) (raylib:vector2-x left)))
          (size-y (- (raylib:vector2-y down) (raylib:vector2-y up))))
      (raylib:copy-vector2 position up)
      (let ((point up))
        (raylib:%vector2-add (& point) (& point) (& origin))
        (raylib:%vector2-add (& point) (& point) (& (scene2d-node-position (scene2d-focusable-content focusable))))
        (raylib:%vector2-add (& point) (& point) (& (scene2d-node-origin (scene2d-focusable-content focusable)))))
      (raylib:copy-vector2 position left)
      (incf (raylib:vector2-x up) (/ size-x 2.0))
      (incf (raylib:vector2-y left) (/ size-y 2.0))
      (scene2d-focusable-update-upper-focal-points focusable size-x size-y))
    (call-next-method)))

(defparameter *scene2d-focus-manager-distance-ratio* 0.999)

(defstruct scene2d-focus-manager
  "A structure that contains a list of SCENE2D-FOCUSABLEs."
  (focusables nil :type list)
  (distance-ratio *scene2d-focus-manager-distance-ratio* :type single-float))

(defun scene2d-focus-manager-focused (manager)
  "Get the currently focused SCENE2D-FOCUSABLE."
  (first (scene2d-focus-manager-focusables manager)))

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
         (let ((focused-point (scene2d-focusable-focal-point focused :right)))
           (multiple-value-bind (lower-bound upper-bound)
               (loop :for focusable :in focusables
                     :minimize (raylib:vector2-x (scene2d-focusable-focal-point focusable :left)) :into lower-bound :of-type single-float
                     :maximize (raylib:vector2-x (scene2d-focusable-focal-point focusable :right)) :into upper-bound :of-type single-float
                     :finally (return (values lower-bound upper-bound)))
             (flet ((directional-distance (focusable &aux (focal-point (scene2d-focusable-focal-point focusable :left)))
                      (let ((distance (+- (raylib:vector2-x focal-point) (raylib:vector2-x focused-point))))
                        (if (minusp distance)
                            (+ (+- (raylib:vector2-x focal-point) lower-bound)
                               (+- upper-bound (raylib:vector2-x focused-point))
                               1.0)
                            distance)))
                    (non-directional-distance (focusable &aux (focal-point (scene2d-focusable-focal-point focusable :left)))
                      (abs (- (raylib:vector2-y focal-point) (raylib:vector2-y focused-point)))))
               (flet ((distance (focusable)
                        (if (and (= (raylib:vector2-x (scene2d-focusable-focal-point focused :left))
                                    (raylib:vector2-x (scene2d-focusable-focal-point focusable :left)))
                                 (= (raylib:vector2-x (scene2d-focusable-focal-point focused :right))
                                    (raylib:vector2-x (scene2d-focusable-focal-point focusable :right))))
                            most-positive-single-float
                            (lerp distance-ratio (non-directional-distance focusable) (directional-distance focusable)))))
                 (setf focusables (sort focusables #'< :key #'distance)))))))))))
