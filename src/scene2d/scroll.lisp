(in-package #:eon)

(defun scene2d-scroll-offset (parent child)
  (clet ((%parent-upper (foreign-alloca '(:struct raylib:vector2)))
         (%child-upper (foreign-alloca '(:struct raylib:vector2))))
    (let ((parent-lower (scene2d-focusable-focus-point parent))
          (parent-size (scene2d-size parent))
          (parent-upper (cobj:pointer-cobject %parent-upper 'raylib:vector2))
          (child-lower (scene2d-focusable-focus-point child))
          (child-size (scene2d-size child))
          (child-upper (cobj:pointer-cobject %child-upper 'raylib:vector2)))
      (raylib:%vector2-add (& parent-upper) (& parent-lower) (& parent-size))
      (raylib:%vector2-add (& child-upper) (& child-lower) (& child-size))
      (macrolet ((symmetric-impl (x-impl &aux (y-impl (copy-tree x-impl)))
                   (subst-swap y-impl
                     (raylib:vector2-x raylib:vector2-y))
                   `(raylib:make-vector2 :x ,x-impl :y ,y-impl)))
        (symmetric-impl
         (cond
           ((< (raylib:vector2-x child-lower) (raylib:vector2-x parent-lower))
            (- (raylib:vector2-x parent-lower) (raylib:vector2-x child-lower)))
           ((< (raylib:vector2-x parent-upper) (raylib:vector2-x child-upper))
            (- (raylib:vector2-x parent-upper) (raylib:vector2-x child-upper)))
           (t 0.0)))))))

(defun scene2d-scroll-region-p (instance)
  (and (scene2d-focusable-p instance)
       (scene2d-scissor-p (scene2d-focusable-content instance))))

(deftype scene2d-scroll-region ()
  '(and scene2d-focusable (satisfies scene2d-scroll-region-p)))

(defun make-scene2d-scroll-region (&rest args &key child size &allow-other-keys)
  (remove-from-plistf args :child :size)
  (apply #'make-scene2d-focusable :content (make-scene2d-scissor :content child :size size) args))

(defun scene2d-scroll-region-child (region)
  (scene2d-scissor-content (scene2d-focusable-content region)))

(defun scene2d-scroll-region-scroll-to-focusable (region focusable)
  (let ((position (scene2d-node-position (scene2d-scroll-region-child region)))
        (offset (scene2d-scroll-offset region focusable)))
    (raylib:%vector2-add (& position) (& position) (& offset))))

(defmethod scene2d-construct-form ((type (eql 'scene2d-scroll-region)) &rest args &key child size &allow-other-keys)
  (declare (ignore child size))
  `(make-scene2d-scroll-region . ,args))

(defun scene2d-box-scroll-region (box &optional (cells 8))
  (scene2d-layout box)
  (make-scene2d-scroll-region :child box
                              :size (ecase (scene2d-box-orientation box)
                                      (:vertical (raylib:make-vector2 :x (raylib:vector2-x (scene2d-size box))
                                                                      :y (loop :for cell :in (scene2d-box-content box)
                                                                               :repeat cells
                                                                               :summing (raylib:vector2-y (scene2d-cell-size cell)) :of-type single-float))))))

(defstruct scene2d-tile-scroll-style
  (tile (load-asset 'raylib:texture +scene2d-window-default-background-texture+ :format :png)))

(defstruct (scene2d-tile-scroll (:include scene2d-layout))
  (style (make-scene2d-tile-scroll-style) :type scene2d-tile-scroll-style))

(defun scene2d-tile-scroll-offset (scroll)
  (scene2d-table-position (scene2d-tile-scroll-content scroll)))

(defmethod scene2d-layout ((scroll scene2d-tile-scroll))
  (loop :with table := (setf (scene2d-tile-scroll-content scroll) (make-scene2d-table))
        :with size := (scene2d-tile-scroll-size scroll)
        :and tile := (scene2d-tile-scroll-style-tile (scene2d-tile-scroll-style scroll))
        :with tile-size := (let ((child (ensure-scene2d-node tile))) (scene2d-layout child) (scene2d-size child))
        :with rows := (1+ (ceiling (raylib:vector2-y size) (raylib:vector2-y tile-size)))
        :and cols := (1+ (ceiling (raylib:vector2-x size) (raylib:vector2-x tile-size)))
        :for row :below rows
        :do (loop :initially (scene2d-table-newline table)
                  :for col :below cols
                  :do (scene2d-table-add-child table (ensure-scene2d-node tile)))
        :finally
           (scene2d-layout table)
           (raylib:%vector2-subtract
            (& (scene2d-tile-scroll-size scroll))
            (& (scene2d-size table)) (& tile-size))))

(defmethod scene2d-draw ((scroll scene2d-tile-scroll) position origin scale rotation tint)
  (let* ((child (scene2d-tile-scroll-content scroll))
         (vbox child)
         (vcells (scene2d-box-content vbox))
         (hbox (scene2d-cell-content (first vcells)))
         (hcells (scene2d-box-content hbox))
         (visible-rows (1- (length vcells)))
         (visible-cols (1- (length hcells)))
         (visible-size (scene2d-tile-scroll-size scroll)))
    (unless (or (zerop (raylib:vector2-x visible-size)) (zerop (raylib:vector2-y visible-size)))
      (let ((tile-width (/ (raylib:vector2-x visible-size) (coerce visible-cols 'single-float)))
            (tile-height (/ (raylib:vector2-y visible-size) (coerce visible-rows 'single-float))))
        (with-accessors ((child-x raylib:vector2-x)
                         (child-y raylib:vector2-y))
            (scene2d-table-position child)
          (let ((original-child-x child-x)
                (original-child-y child-y))
            (setf child-x (- (mod child-x tile-width) tile-width)
                  child-y (- (mod child-y tile-height) tile-height))
            (call-next-method)
            (setf child-x original-child-x
                  child-y original-child-y)))))))

(defun scene2d-tile-scroll-region-p (instance)
  (and (scene2d-scissor-p instance)
       (scene2d-tile-scroll-p (scene2d-scissor-content instance))))

(deftype scene2d-tile-scroll-region ()
  '(and scene2d-scissor (satisfies scene2d-tile-scroll-region-p)))

(defstruct scene2d-tile-scroll-region-style
  (tile-scroll-style (make-scene2d-tile-scroll-style) :type scene2d-tile-scroll-style))

(defun make-scene2d-tile-scroll-region (&rest args
                                          &key
                                            (size (raylib:make-vector2 :x 100.0 :y 100.0))
                                            (style (make-scene2d-tile-scroll-region-style))
                                          &allow-other-keys)
  (remove-from-plistf args :style)
  (apply #'make-scene2d-scissor :content (make-scene2d-tile-scroll
                                          :style (scene2d-tile-scroll-region-style-tile-scroll-style style)
                                          :size (raylib:copy-vector2 size))
                                :size size args))

(defun scene2d-tile-scroll-region-offset (region)
  (scene2d-tile-scroll-offset (scene2d-scissor-content region)))
