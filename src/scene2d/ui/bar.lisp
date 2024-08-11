(in-package #:eon)

(defstruct progress-bar-style
  "A structure describing the style of PROGRESS-BAR."
  (orientation :horizontal :type (member :vertical :horizontal))
  (track nil :type t)
  (track-region nil :type (or raylib:rectangle null))
  (thumb nil :type t)
  (thumb-alignment :center :type (member :center :edge))
  (fill nil :type t))

(define-scene2d-default-construct-form progress-bar-style (orientation track track-region thumb thumb-alignment fill))

(defstruct (progress-bar (:include scene2d-container)
                       (:constructor %make-progress-bar))
  (style (make-progress-bar-style) :type progress-bar-style))

(defmethod scene2d-size ((progress-bar progress-bar))
  (scene2d-size (first (progress-bar-content progress-bar))))

(defmacro with-progress-bar-value ((progress progress-bar) &body body)
  (with-gensyms (region track fill thumb style track-region update-thumb-region size value position)
    `(destructuring-bind (,track ,fill ,thumb &aux (,style (progress-bar-style ,progress-bar))) (progress-bar-content ,progress-bar)
       (clet ((,region (foreign-alloca '(:struct raylib:rectangle))))
         (if-let ((,track-region (progress-bar-style-track-region ,style)))
           (clet ((,track-region (cthe (:pointer (:struct raylib:rectangle)) (& ,track-region))))
             (csetf ([] ,region) ([] ,track-region)))
           (let ((,size (scene2d-size ,track)))
             (setf (-> ,region raylib:x) 0.0
                   (-> ,region raylib:y) 0.0
                   (-> ,region raylib:width) (raylib:vector2-x ,size)
                   (-> ,region raylib:height) (raylib:vector2-y ,size))))
         (let ((,position (scene2d-position ,fill)))
           (setf (raylib:vector2-x ,position) (-> ,region raylib:x)
                 (raylib:vector2-y ,position) (-> ,region raylib:y)))
         ,(let* ((horizontal-impl
                   `(progn
                      (let ((,position (scene2d-position ,thumb)))
                        (labels ((,update-thumb-region ()
                                   (case (progress-bar-style-thumb-alignment ,style)
                                     (:edge
                                      (let ((,size (raylib:vector2-multiply (scene2d-size ,thumb) (scene2d-scale ,thumb))))
                                        (incf (-> ,region raylib:x) (/ (raylib:vector2-x ,size) 2.0))
                                        (decf (-> ,region raylib:width) (raylib:vector2-x ,size))))))
                                 (,progress ()
                                   (,update-thumb-region)
                                   (/ (- (raylib:vector2-x ,position) (-> ,region raylib:x)) (-> ,region raylib:width)))
                                 ((setf ,progress) (,value &aux (,size (scene2d-size ,fill)))
                                   (setf (raylib:vector2-x ,size) (* (-> ,region raylib:width) ,value)
                                         (raylib:vector2-y ,size) (-> ,region raylib:height)
                                         (scene2d-size ,fill) ,size)
                                   (,update-thumb-region)
                                   (setf (raylib:vector2-x ,position) (+ (-> ,region raylib:x) (* ,value (-> ,region raylib:width)))
                                         (raylib:vector2-y ,position) (+ (-> ,region raylib:y) (/ (-> ,region raylib:height) 2.0)))
                                   ,value))
                          (declare (inline ,update-thumb-region ,progress (setf ,progress))
                                   (ignorable #',progress #'(setf ,progress)))
                          (symbol-macrolet ((,progress (,progress))) . ,body)))))
                 (vertical-impl (copy-tree horizontal-impl))
                 (vertical-impl (subst-swap vertical-impl
                                  (raylib:x raylib:y)
                                  (raylib:width raylib:height)
                                  (raylib:vector2-x raylib:vector2-y))))
            `(ecase (progress-bar-style-orientation ,style)
               (:horizontal ,horizontal-impl)
               (:vertical ,vertical-impl)))))))

(defun progress-bar-value (progress-bar)
  "Get the progress value of PROGRESS-BAR."
  (with-progress-bar-value (progress progress-bar)
    progress))

(defun (setf progress-bar-value) (value progress-bar)
  "Set the progress value of PROGRESS-BAR."
  (with-progress-bar-value (progress progress-bar)
    (setf progress value)))

(defun make-progress-bar (&rest args &key style (value 0.0) &allow-other-keys)
  (remove-from-plistf args :value)
  (let ((progress-bar (apply #'%make-progress-bar
                             :content (list (ensure-scene2d-node (progress-bar-style-track style))
                                            (ensure-scene2d-node (progress-bar-style-fill style))
                                            (ensure-scene2d-node-origin-at-center
                                             (ensure-scene2d-node (progress-bar-style-thumb style))))
                           args)))
    (setf (progress-bar-value progress-bar) value)
    progress-bar))

(define-scene2d-default-construct-form progress-bar (style value))
