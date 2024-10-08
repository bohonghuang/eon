(in-package #:eon)

(defvar *tiled-tileset-texture-table* nil)

(defmacro with-tiled-tileset-texture-table (&body body)
  `(let ((*tiled-tileset-texture-table* (or *tiled-tileset-texture-table* (make-hash-table)))) . ,body))

(defvar *tiled-renderer-camera* nil
  "A variable that needs to be bound to a CAMERA-2D when creating a TILED-RENDERER if optimization of rendering performance is required based on the camera's view.")

(defun tiled-tileset-texture (tileset)
  (let ((table (or *tiled-tileset-texture-table* (make-hash-table)))
        (image (tiled:tileset-image tileset)))
    (ensure-gethash
     tileset table
     (etypecase image
       (tiled:external-tiled-image
        (load-asset 'raylib:texture (tiled:image-source image)))))))

(defun tiled-tile-texture-region (tile)
  (etypecase tile
    (tiled:tiled-tile
     (let* ((tileset (tiled:tile-tileset tile))
            (tile-width (tiled:tileset-tile-width tileset))
            (tile-height (tiled:tileset-tile-height tileset)))
       (declare (type non-negative-fixnum tile-width tile-height))
       (let ((tile-x (tiled:tile-pixel-x tile))
             (tile-y (tiled:tile-pixel-y tile)))
         (declare (type non-negative-fixnum tile-x tile-y))
         (make-texture-region
          :texture (tiled-tileset-texture tileset)
          :region (raylib:make-rectangle
                   :x (coerce tile-x 'single-float)
                   :y (coerce tile-y 'single-float)
                   :width (coerce tile-width 'single-float)
                   :height (coerce tile-height 'single-float))))))))

(deftype tiled-renderer ()
  "A function that accepts the same drawing parameters as SCENE2D-DRAW, used to draw a map or one of its layers."
  `(function (&optional raylib:vector2 raylib:vector2 raylib:vector2 single-float raylib:color)))

(declaim (ftype (function (list) (values tiled-renderer)) tiled-compose-renderers))
(defun tiled-compose-renderers (renderers)
  (lambda (&rest args)
    (declare (dynamic-extent args))
    (dolist (renderer renderers)
      (declare (type function renderer))
      (apply renderer args))))

(declaim (ftype (function (tiled:layer) (values tiled-renderer)) tiled-layer-renderer))
(defun tiled-layer-renderer (layer)
  "Create a TILED-RENDERER for LAYER of type CL-TILED:LAYER and return it."
  (with-tiled-tileset-texture-table
    (typecase layer
      (tiled:group-layer
       (tiled-compose-renderers (mapcar #'tiled-layer-renderer (tiled:group-layers layer))))
      (tiled:tile-layer
       (multiple-value-bind (layer-offset-x layer-offset-y) (tiled:layer-full-offsets layer)
         (declare (type fixnum layer-offset-x layer-offset-y))
         (let* ((map (tiled:layer-map layer))
                (tile-width (tiled:map-tile-width map))
                (tile-height (tiled:map-tile-width map))
                (layer-width (tiled:map-width-pixels map))
                (layer-height (tiled:map-height-pixels map))
                (dest (raylib:make-rectangle))
                (offset (cobj:pointer-cobject (cobj:cobject-pointer dest) 'raylib:vector2))
                (position +vector2-zeros+)
                (origin +vector2-zeros+)
                (scale +vector2-ones+)
                (rotation 0.0)
                (color raylib:+white+))
           (declare (type non-negative-fixnum tile-width tile-height layer-width layer-height))
           (let* ((animated-tiles nil)
                  (offset-regions
                    (loop :with tile-texture-regions := (make-hash-table)
                          :for cell :of-type tiled:cell :in (tiled:layer-cells layer)
                          :for cell-column :of-type non-negative-fixnum := (tiled:cell-column cell)
                          :for cell-row :of-type non-negative-fixnum := (tiled:cell-row cell)
                          :for cell-x :of-type fixnum := (+ (the non-negative-fixnum (* cell-column tile-width)) layer-offset-x)
                          :for cell-y :of-type fixnum := (+ (the non-negative-fixnum (* cell-row tile-height)) layer-offset-y)
                          :for tile :of-type tiled:tiled-tile := (tiled:cell-tile cell)
                          :for offset-region := (cons (cons (coerce cell-x 'single-float) (coerce cell-y 'single-float))
                                                      (ensure-gethash
                                                       tile tile-texture-regions
                                                       (let ((texture-region-cons (cons (tiled-tile-texture-region tile) nil)))
                                                         (when (typep tile 'tiled:animated-tile)
                                                           (push (cons texture-region-cons
                                                                       (nreverse
                                                                        (loop :for frame :in (tiled:tile-frames tile)
                                                                              :sum (* (coerce (the non-negative-fixnum (tiled:frame-duration frame)) 'single-float) 0.001) :into duration :of-type single-float
                                                                              :collect (cons duration (tiled-tile-texture-region (tiled:frame-tile frame))))))
                                                                 animated-tiles))
                                                         texture-region-cons)))
                          :when (cadr offset-region) :collect offset-region)))
             (multiple-value-bind (bound-predicate position-predicate)
                 (if-let ((camera *tiled-renderer-camera*))
                   (let ((target (raylib:camera-2d-target camera))
                         (offset (raylib:camera-2d-offset camera)))
                     (symbol-macrolet ((zoom (raylib:camera-2d-zoom camera)))
                       (let ((|(+ (raylib:vector2-x target) (/ (raylib:vector2-x offset) zoom))| 0.0)
                             (|(- (raylib:vector2-x target) (/ (raylib:vector2-x offset) zoom) (* (coerce tile-width 'single-float) (raylib:vector2-x scale)))| 0.0)
                             (|(+ (raylib:vector2-y target) (/ (raylib:vector2-y offset) zoom))| 0.0)
                             (|(- (raylib:vector2-y target) (/ (raylib:vector2-y offset) zoom) (* (coerce tile-height 'single-float) (raylib:vector2-y scale)))| 0.0))
                         (declare (type single-float
                                        |(+ (raylib:vector2-x target) (/ (raylib:vector2-x offset) zoom))|
                                        |(- (raylib:vector2-x target) (/ (raylib:vector2-x offset) zoom) (* (coerce tile-width 'single-float) (raylib:vector2-x scale)))|
                                        |(+ (raylib:vector2-y target) (/ (raylib:vector2-y offset) zoom))|
                                        |(- (raylib:vector2-y target) (/ (raylib:vector2-y offset) zoom) (* (coerce tile-height 'single-float) (raylib:vector2-y scale)))|))
                         (values
                          (lambda (position origin scale rotation tint)
                            (declare (ignore origin rotation tint))
                            (setf |(+ (raylib:vector2-x target) (/ (raylib:vector2-x offset) zoom))|
                                  (+ (raylib:vector2-x target) (/ (raylib:vector2-x offset) zoom))
                                  |(- (raylib:vector2-x target) (/ (raylib:vector2-x offset) zoom) (* (coerce tile-width 'single-float) (raylib:vector2-x scale)))|
                                  (- (raylib:vector2-x target) (/ (raylib:vector2-x offset) zoom) (* (coerce tile-width 'single-float) (raylib:vector2-x scale)))
                                  |(+ (raylib:vector2-y target) (/ (raylib:vector2-y offset) zoom))|
                                  (+ (raylib:vector2-y target) (/ (raylib:vector2-y offset) zoom))
                                  |(- (raylib:vector2-y target) (/ (raylib:vector2-y offset) zoom) (* (coerce tile-height 'single-float) (raylib:vector2-y scale)))|
                                  (- (raylib:vector2-y target) (/ (raylib:vector2-y offset) zoom) (* (coerce tile-height 'single-float) (raylib:vector2-y scale))))
                            (and
                             (>= |(+ (raylib:vector2-x target) (/ (raylib:vector2-x offset) zoom))|
                                 (+ (raylib:vector2-x position) (* (coerce layer-offset-x 'single-float)
                                                                   (raylib:vector2-x scale))))
                             (>= |(+ (raylib:vector2-y target) (/ (raylib:vector2-y offset) zoom))|
                                 (+ (raylib:vector2-y position) (* (coerce layer-offset-y 'single-float)
                                                                   (raylib:vector2-y scale))))
                             (>= (+ (raylib:vector2-x position) (* (+ (coerce layer-offset-x 'single-float)
                                                                      (coerce layer-width 'single-float))
                                                                   (raylib:vector2-x scale)))
                                 (- (raylib:vector2-x target) (/ (raylib:vector2-x offset) zoom)))
                             (>= (+ (raylib:vector2-y position) (* (+ (coerce layer-offset-y 'single-float)
                                                                      (coerce layer-height 'single-float))
                                                                   (raylib:vector2-y scale)))
                                 (- (raylib:vector2-y target) (/ (raylib:vector2-y offset) zoom)))))
                          (lambda (position origin scale rotation tint)
                            (declare (ignore origin scale rotation tint))
                            (not
                             (or
                              (< |(+ (raylib:vector2-x target) (/ (raylib:vector2-x offset) zoom))| (raylib:vector2-x position))
                              (< (raylib:vector2-x position) |(- (raylib:vector2-x target) (/ (raylib:vector2-x offset) zoom) (* (coerce tile-width 'single-float) (raylib:vector2-x scale)))|)
                              (< |(+ (raylib:vector2-y target) (/ (raylib:vector2-y offset) zoom))| (raylib:vector2-y position))
                              (< (raylib:vector2-y position) |(- (raylib:vector2-y target) (/ (raylib:vector2-y offset) zoom) (* (coerce tile-height 'single-float) (raylib:vector2-y scale)))|))))))))
                   (values (constantly t) (constantly t)))
               (declare (type (function (raylib:vector2 raylib:vector2 raylib:vector2 single-float raylib:color) (values boolean)) position-predicate bound-predicate))
               (lambda (&optional
                          (position position)
                          (origin origin)
                          (scale scale)
                          (rotation rotation)
                          (tint color))
                 (when (funcall bound-predicate position origin scale rotation tint)
                   (loop :for (texture-region-cons . frames) :in animated-tiles
                         :for current-duration :of-type single-float
                           := (coerce (mod (game-loop-time) (coerce (the single-float (car (first frames))) 'double-float)) 'single-float)
                         :do (setf (car texture-region-cons) (loop :for current-texture-region :of-type texture-region := (cdr (first frames)) :then texture-region
                                                                   :for (duration . texture-region) :of-type (single-float . texture-region) :in frames
                                                                   :if (< duration current-duration)
                                                                     :return current-texture-region
                                                                   :finally (return current-texture-region))))
                   (setf (raylib:rectangle-width dest) (* (coerce tile-width 'single-float) (raylib:vector2-x scale))
                         (raylib:rectangle-height dest) (* (coerce tile-height 'single-float) (raylib:vector2-y scale)))
                   (loop :for ((offset-x . offset-y) . (region . nil)) :in offset-regions
                         :do (setf (raylib:rectangle-x dest) (+ (raylib:vector2-x position) (* (the single-float offset-x) (raylib:vector2-x scale)))
                                   (raylib:rectangle-y dest) (+ (raylib:vector2-y position) (* (the single-float offset-y) (raylib:vector2-y scale))))
                         :when (funcall position-predicate offset origin scale rotation tint)
                           :do (raylib:draw-texture-pro (texture-region-texture region) (texture-region-region region) dest origin rotation tint)))))))))
      (t (constantly nil)))))

(declaim (ftype (function (tiled:tiled-map) (values tiled-renderer list)) tiled-map-renderer))
(defun tiled-map-renderer (map)
  "Create a TILED-RENDERER for MAP of type CL-TILED:TILED-MAP and return it."
  (with-tiled-tileset-texture-table
    (let* ((layers (tiled:map-layers map))
           (renderers (mapcar #'tiled-layer-renderer layers)))
      (values (tiled-compose-renderers renderers) renderers))))

(defmethod load-asset ((asset-type (eql 'tiled:tiled-map)) (path pathname) &key)
  (tiled:load-map path))

(defmethod unload-asset ((asset tiled:tiled-map))
  (declare (ignore asset)))
