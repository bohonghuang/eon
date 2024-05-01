(in-package #:eon)

(declaim (inline %make-texture-region))
(defstruct (texture-region (:constructor %make-texture-region))
  "A structure defining a rectangular area of a texture, whose origin is at the top left corner."
  (texture (raylib:make-texture :id 0 :width 1 :height 1 :mipmaps 0 :format 0) :type raylib:texture)
  (region (raylib:make-rectangle :x 0.0 :y 0.0 :width 0.0 :height 0.0) :type raylib:rectangle))

(declaim (inline make-texture-region))
(defun make-texture-region (&key texture region)
  "Construct a TEXTURE-REGION from TEXTURE of type RAYLIB:TEXTURE and REGION of type RAYLIB:RECTANGLE. REGION can be NIL to include the entire area of TEXTURE. Note that negative-sized REGION is also valid and will flip the corresponding area of TEXTURE."
  (%make-texture-region :texture texture
                        :region (or region (raylib:make-rectangle :x 0.0
                                                                  :y 0.0
                                                                  :width (coerce (raylib:texture-width texture) 'single-float)
                                                                  :height (coerce (raylib:texture-height texture) 'single-float)))))

(declaim (inline texture-region-width))
(defun texture-region-width (texture-region)
  "Get the width of TEXTURE-REGION."
  (abs (raylib:rectangle-width (texture-region-region texture-region))))

(declaim (inline texture-region-height))
(defun texture-region-height (texture-region)
  "Get the height of TEXTURE-REGION."
  (abs (raylib:rectangle-height (texture-region-region texture-region))))

(declaim (ftype (function (raylib:texture (cons positive-fixnum (cons positive-fixnum null)))
                          (values (simple-array texture-region (* *))))
                split-texture))
(defun split-texture (texture dimensions)
  "Divide RAYLIB:TEXTURE into a two-dimensional grid and return the corresponding SIMPLE-ARRAY of TEXTURE-REGION with DIMENSIONS (a list of 2 POSITIVE-FIXNUMs)."
  (destructuring-bind (rows cols) dimensions
    (declare (type positive-fixnum rows cols))
    (let ((height (raylib:texture-height texture))
          (width (raylib:texture-width texture)))
      (declare (type positive-fixnum height width))
      (unless (zerop (rem width cols))
        (error "The width of the image (i.e. ~D) cannot be evenly divided by the number of columns (i.e. ~D)." width cols))
      (unless (zerop (rem height rows))
        (error "The height of the image (i.e. ~D) cannot be evenly divided by the number of rows (i.e. ~D)." height rows))
      (loop :with array :of-type (simple-array texture-region (* *)) := (make-array (list rows cols))
            :with region-height :of-type positive-fixnum := (truncate height rows)
            :and region-width :of-type positive-fixnum := (truncate width cols)
            :for region-y :of-type non-negative-fixnum :below height :by region-height
            :for row :of-type non-negative-fixnum :from 0
            :do (loop :for region-x :of-type non-negative-fixnum :below width :by region-width
                      :for col :of-type non-negative-fixnum :from 0
                      :do (setf (aref array row col)
                                (make-texture-region
                                 :texture texture
                                 :region (raylib:make-rectangle
                                          :x (coerce region-x 'single-float)
                                          :y (coerce region-y 'single-float)
                                          :width (coerce region-width 'single-float)
                                          :height (coerce region-height 'single-float)))))
            :finally (return array)))))
