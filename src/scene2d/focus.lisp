(in-package #:eon)

(defstruct (scene2d-focusable (:include scene2d-container))
  (focus-point (raylib:make-vector2)))

(defmethod scene2d-draw ((focusable scene2d-focusable) position origin scale rotation tint)
  (let ((point (scene2d-focusable-focus-point focusable)))
    (raylib:copy-vector2 position point)
    (raylib:%vector2-add (& point) (& point) (& origin))
    (raylib:%vector2-add (& point) (& point) (& (scene2d-node-position (scene2d-focusable-content focusable))))
    (raylib:%vector2-add (& point) (& point) (& (scene2d-node-origin (scene2d-focusable-content focusable))))
    (call-next-method)))

(defstruct scene2d-focus-manager
  (focusables nil :type list))

(defun scene2d-focus-manager-focused (manager)
  (first (scene2d-focus-manager-focusables manager)))

(defun scene2d-focus-manager-handle-key (manager key)
  (with-accessors ((focusables scene2d-focus-manager-focusables))
      manager
    (let* ((focused (first focusables))
           (focused-point (scene2d-focusable-focus-point focused)))
      (macrolet ((symmetric-impl (&body right-impl &aux
                                                     (left-impl (copy-tree right-impl))
                                                     (up-impl (copy-tree right-impl))
                                                     (down-impl (copy-tree right-impl)))
                   (subst-swap left-impl
                     (:minimize :maximize)
                     (most-positive-single-float most-negative-single-float)
                     (<= >=))
                   (subst-swap up-impl
                     (:minimize :maximize)
                     (most-positive-single-float most-negative-single-float)
                     (<= >=)
                     (raylib:vector2-x raylib:vector2-y))
                   (subst-swap down-impl
                     (raylib:vector2-x raylib:vector2-y))
                   `(ecase key
                      (:up . ,up-impl)
                      (:down . ,down-impl)
                      (:left . ,left-impl)
                      (:right . ,right-impl))))
        (symmetric-impl
         (multiple-value-bind (focus-lower-bound focus-upper-bound)
             (loop :for focusable :in focusables
                   :for bound := (raylib:vector2-x (scene2d-focusable-focus-point focusable))
                   :minimize bound :into lower-bound :of-type single-float
                   :maximize bound :into upper-bound :of-type single-float
                   :finally (return (values lower-bound upper-bound)))
           (let (candidates non-candidates)
             (if (= (raylib:vector2-x focused-point) focus-upper-bound)
                 (setf non-candidates (delete-if (lambda (focusable)
                                                   (when (= (raylib:vector2-x (scene2d-focusable-focus-point focusable)) focus-lower-bound)
                                                     (push focusable candidates)))
                                                 focusables))
                 (flet ((candidate-valid-p (candidate)
                          (not (<= (raylib:vector2-x (scene2d-focusable-focus-point candidate)) (raylib:vector2-x focused-point)))))
                   (setf focusables (sort focusables #'<
                                          :key (lambda (focusable)
                                                 (raylib:%vector2-distance (& focused-point) (& (scene2d-focusable-focus-point focusable))))))
                   (let ((valid-min-distance (raylib:%vector2-distance (& focused-point) (& (scene2d-focusable-focus-point (find-if #'candidate-valid-p focusables))))))
                     (setf non-candidates (delete-if (lambda (focusable)
                                                       (when (candidate-valid-p focusable)
                                                         (when (= (raylib:%vector2-distance (& (scene2d-focusable-focus-point focusable)) (& focused-point)) valid-min-distance)
                                                           (push focusable candidates))))
                                                     focusables)))))
             (loop :with offset := (raylib:make-vector2)
                   :with candidate-cons := candidates
                   :and candidate-prev-cons := nil
                   :for prev-cons := candidates :then cons
                   :for cons :on (cdr candidates)
                   :for point := (scene2d-focusable-focus-point (car cons))
                   :for candidate-point := (scene2d-focusable-focus-point (car candidate-cons))
                   :do (raylib:%vector2-subtract (& offset) (& point) (& focused-point))
                   :when (< (abs (raylib:vector2-y offset))
                            (progn
                              (raylib:%vector2-subtract (& offset) (& candidate-point) (& focused-point))
                              (abs (raylib:vector2-y offset))))
                     :do (setf candidate-cons cons candidate-prev-cons prev-cons)
                   :finally
                      (when candidate-prev-cons
                        (shiftf (cdr candidate-prev-cons) (cdr candidate-cons) candidates candidate-cons)))
             (setf focusables (nconc candidates non-candidates)))))))))
