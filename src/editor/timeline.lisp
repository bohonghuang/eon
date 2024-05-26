(in-package #:eon.editor)

(defgeneric edit-timeline-form (type &rest args))

(defun edit-timeline (form)
  (if form
      (apply #'edit-timeline-form (car form) (cdr form))
      (with-received-preview-function
        (async
          (when-let ((type (await (promise-selection "Select the tween type." '(:to :from :sequence :parallel :tween :call)))))
            (with-sent-preview-function ()
              (await (edit-timeline-form type))))))))

(defmethod edit-timeline-form :around (type &rest args)
  (with-received-preview-function
    (let (operations)
      (push (cons 'edit (lambda (form) (declare (ignore form)) (with-sent-preview-function () (call-next-method)))) operations)
      (push (cons 'wrap (lambda (form)
                          (async
                            (if-let ((parent (await (promise-selection "Select the parent." '(:sequence :parallel)))))
                              (list parent (cons type args)) form))))
            operations)
      (when (and (member type '(:sequence :parallel)) args)
        (push (cons 'unwrap (lambda (form) (declare (ignore form)) (async (first args)))) operations))
      (nreversef operations)
      (if (or (= (length operations) 1) (not (controller-button-down-p :l2)))
          (funcall (cdr (first operations)) (cons type args))
          (async
            (await (funcall (or (assoc-value operations (await (promise-selection
                                                                (format nil "Select an operation for ~A." type)
                                                                (mapcar #'car operations))))
                                (constantly (async (cons type args))))
                            (cons type args))))))))

(defmethod edit-timeline-form ((type (eql 'ute:timeline)) &rest args)
  (with-received-preview-function
    (let ((self (car args)))
      (async
        (do-non-nil (selection
                     (let ((properties `((:self . ,self))))
                       (await (promise-selection (format nil "Edit ~A." type) properties)))
                     (list type self))
          (ecase selection
            (:self (with-sent-preview-function ((list type self) self)
                     (setf self (await (edit-timeline self)))))))))))

(defun tween-debug-container (form)
  `(:tween (ute:timeline ,form)))

(defun edit-timeline-sequence-parallel (type &rest children)
  (with-received-preview-function
    (async
      (do-non-nil (selection
                   (let ((properties `((:children . ,children))))
                     (preview (tween-debug-container (cons type children)))
                     (await (promise-selection (format nil "Edit ~A." type) properties)))
                   (cons type children))
        (ecase selection
          (:children
           (with-sent-preview-function ((cons type children) children)
             (setf children (await (edit-children #'edit-timeline children))))))))))

(defmethod edit-timeline-form ((type (eql :sequence)) &rest children)
  (apply #'edit-timeline-sequence-parallel type children))

(defmethod edit-timeline-form ((type (eql :parallel)) &rest children)
  (apply #'edit-timeline-sequence-parallel type children))

(defmethod edit-timeline-form ((type (eql :tween)) &rest args)
  (destructuring-bind (&optional (tween '(ute:tween))) args
    (async (list type (await (edit-expression tween))))))

(defmethod edit-timeline-form ((type (eql :call)) &rest args)
  (destructuring-bind (&optional (function '(lambda ()))) args
    (async (list type (await (edit-expression function))))))

(setf (fdefinition 'dummy-float) (setf (fdefinition '(setf dummy-float)) (constantly 0.0)))

(defun edit-value-expression (value)
  (async
    (or (await
         (cond
           ((controller-button-down-p :r3) (edit-expression value))
           ((integerp value) (edit-integer value))
           ((floatp value) (edit-float value))
           ((stringp value) (promise-input-text "Enter a string." value))
           (t (edit-expression value))))
        value)))

(defvar *eval-timeline-bindings* nil)

(defun eval-timeline (form &aux (bindings *eval-timeline-bindings*))
  (with-gensyms (value)
    (loop :for (binding . accessor) :in bindings
          :for name := (gensym "ACCESSOR")
          :when (symbolp binding)
            :nconc `((,name () (funcall ,accessor)) ((setf ,name) (,value) (funcall ,accessor ,value)))
              :into definitions
              :and :collect `(,binding (,name)) :into macrobindings
          :when (and (listp binding) (notevery (compose (rcurry #'member
                                                                (list (find-package :cl)
                                                                      (find-package :alexandria)))
                                                        #'symbol-package)
                                               binding))
            :nconc `((,(if (cdr binding) binding (car binding)) (&rest args) (apply ,accessor args)))
              :into definitions
          :finally (return (eval `(flet ,definitions (symbol-macrolet ,macrobindings ,form)))))))

(defun edit-timeline-from-to (type &rest args)
  (with-received-preview-function
    (unless args (setf args (list nil)))
    (destructuring-bind ((&optional
                            places
                          &rest
                            targets)
                         &key
                           duration
                           delay
                           repeat
                           ease)
        args
      (flet ((adjust-targets (places targets &aux (nplaces (length places)) (ntargets (length (first targets))))
               (when targets
                 (cond
                   ((< nplaces ntargets)
                    (loop :for target :in targets :collect (subseq target 0 nplaces)))
                   ((> nplaces ntargets)
                    (loop :for target :in targets :collect (nconc target (make-list (- nplaces ntargets) :initial-element 0.0))))
                   (t targets))))
             (ensure-combined-properties (form)
               (async
                 (let ((property (eval-timeline form)))
                   (typecase property
                     (scene2d-node
                      (let ((accessor (or (await (promise-selection "Select an accessor."
                                                                    '(scene2d-position
                                                                      scene2d-scale
                                                                      scene2d-color
                                                                      scene2d-rotation
                                                                      scene2d-origin
                                                                      scene2d-size)))
                                          'scene2d-position)))
                        (setf property (funcall accessor property)
                              form (list accessor form))))
                     (scene3d-node
                      (let ((accessor (or (await (promise-selection "Select an accessor."
                                                                    '(scene3d-position
                                                                      scene3d-scale
                                                                      scene3d-color
                                                                      scene3d-rotation)))
                                          'scene3d-position)))
                        (setf property (funcall accessor property)
                              form (list accessor form)))))
                   (etypecase property
                     (raylib:vector2 `((raylib:vector2-x ,form) (raylib:vector2-y ,form)))
                     (raylib:vector3 `((raylib:vector3-x ,form) (raylib:vector3-y ,form) (raylib:vector3-z ,form)))
                     (raylib:vector4 `((raylib:vector4-x ,form) (raylib:vector4-y ,form) (raylib:vector4-z ,form) (raylib:vector4-w ,form)))
                     (raylib:color `((integer-float (raylib:color-r ,form)) (integer-float (raylib:color-g ,form))
                                     (integer-float (raylib:color-b ,form)) (integer-float (raylib:color-a ,form))))
                     (raylib:camera-3d `((raylib:vector3-x (raylib:camera-3d-position ,form))
                                         (raylib:vector3-y (raylib:camera-3d-position ,form))
                                         (raylib:vector3-z (raylib:camera-3d-position ,form))
                                         (raylib:vector3-x (raylib:camera-3d-target ,form))
                                         (raylib:vector3-y (raylib:camera-3d-target ,form))
                                         (raylib:vector3-z (raylib:camera-3d-target ,form))
                                         (raylib:vector3-x (raylib:camera-3d-up ,form))
                                         (raylib:vector3-y (raylib:camera-3d-up ,form))
                                         (raylib:vector3-z (raylib:camera-3d-up ,form))))
                     (integer `((integer-float ,form)))
                     (float `(,form))))))
             (place-values () (eval-timeline (cons 'list places)))
             ((setf place-values) (values) (eval-timeline (cons 'setf (mapcan #'list places values))))
             (result ()
               (unless places
                 (setf places (list '(dummy-float))))
               (unless targets
                 (setf targets (list (make-list (length places) :initial-element 0.0))))
               (unless duration
                 (setf duration 1.0))
               (unless delay
                 (setf delay 0.0))
               (unless repeat
                 (setf repeat 0))
               (unless ease
                 (setf ease ''ute:linear-inout))
               (list type (cons places targets) :duration duration :delay delay :repeat repeat :ease ease)))
        (async
          (do-non-nil (selection
                       (let ((properties (progn
                                           (preview (tween-debug-container (result)))
                                           (list (cons :places places)
                                                 (cons :targets targets)
                                                 (cons :duration duration)
                                                 (cons :delay delay)
                                                 (cons :repeat repeat)
                                                 (cons :ease ease)))))
                         (await (promise-selection (format nil "Edit tween ~A." type) properties)))
                       (result))
            (ecase selection
              (:places
               (setf places (if (controller-button-down-p :r3)
                                (await (ensure-combined-properties (await (edit-expression))))
                                (with-sent-preview-function ((tween-debug-container (result)) places)
                                  (await (edit-children #'edit-expression places))))
                     targets (adjust-targets places targets)))
              (:targets
               (if (controller-button-down-p :l3)
                   (setf (lastcar targets) (place-values))
                   (setf targets (with-sent-preview-function ((tween-debug-container (result)) targets)
                                   (await (edit-children (lambda (children)
                                                           (with-received-preview-function
                                                             (unless children
                                                               (setf children (make-list (length places) :initial-element 0.0)))
                                                             (with-sent-preview-function (children (place-values))
                                                               (let ((place-values (place-values)))
                                                                 (async
                                                                   (prog1 (await (edit-children #'edit-value-expression children nil))
                                                                     (setf (place-values) place-values)))))))
                                                         targets))))))
              (:duration
               (setf duration (with-sent-preview-function ((tween-debug-container (result)) duration)
                                (await (edit-value-expression duration)))))
              (:delay
               (setf delay (with-sent-preview-function ((tween-debug-container (result)) delay)
                             (await (edit-value-expression delay)))))
              (:repeat
               (setf repeat (with-sent-preview-function ((tween-debug-container (result)) repeat)
                              (await (edit-value-expression repeat)))))
              (:ease
               (setf ease (list 'quote (or (await (promise-selection "Select an easing function." ute::+tween-equations+)) (second ease))))))
            (result)))))))

(defmethod edit-timeline-form ((type (eql :from)) &rest children)
  (apply #'edit-timeline-from-to type children))

(defmethod edit-timeline-form ((type (eql :to)) &rest children)
  (apply #'edit-timeline-from-to type children))

(defparameter *timeline-overview-timescale* 1.0)

(defun timeline-clip (duration &optional (offset 0.0))
  (scene2d-construct (scene2d-margin :child (debug-container :child (scene2d-cell :size (raylib:make-vector2
                                                                                         :x (* duration *timeline-overview-timescale*)
                                                                                         :y 2.0))
                                                             :border-color (raylib:make-color :r (random 127) :g (random 127) :b (random 127) :a 255))
                                     :left (* offset *timeline-overview-timescale*):top 1.0 :bottom 1.0)))

(defun timeline-overview (form)
  (destructuring-ecase form
    ((ute:timeline form &rest args)
     (declare (ignore args))
     (timeline-overview form))
    ((:pause duration)
     (timeline-clip duration))
    (((:to :from) places &key (duration 0.0) (delay 0.0) (repeat 0) &allow-other-keys)
     (declare (ignore places))
     (timeline-clip (* duration (1+ repeat)) delay))
    ((:sequence &rest children)
     (scene2d-construct (scene2d-box :children (mapcar #'timeline-overview children) :alignment (:start :center) :orientation :horizontal)))
    ((:parallel &rest children)
     (scene2d-construct (scene2d-box :children (mapcar #'timeline-overview children) :alignment (:start :center) :orientation :vertical)))
    ((:tween form)
     (if (typep form '(cons (eql ute:timeline) t))
         (scene2d-construct (debug-container :child (timeline-overview form)))
         (timeline-clip (ute:full-duration (eval-timeline form)))))
    ((:call function)
     (declare (ignore function))
     (timeline-clip 0.0))))

(defun timeline-overview-window (form)
  (let (status-label)
    (values
     (screen-cell
      (scene2d-construct
       (scene2d-window
        :child (let ((temp-widget (let ((*timeline-overview-timescale* 1.0))
                                    (timeline-overview form))))
                 (scene2d-layout temp-widget)
                 (let ((duration (raylib:vector2-x (scene2d-size temp-widget))))
                   (scene2d-construct
                    (scene2d-box
                     :children (list (block construct-widget
                                       (let ((*timeline-overview-timescale*
                                               (if (zerop duration)
                                                   (return-from construct-widget
                                                     (scene2d-construct
                                                      (scene2d-cell :size (raylib:make-vector2
                                                                           :x (/ (coerce +world-viewport-default-width+ 'single-float) 2.0)
                                                                           :y 0.0))))
                                                   (/ (/ (coerce +world-viewport-default-width+ 'single-float) 2.0) duration))))
                                         (scene2d-construct (scene2d-margin :bottom 2.0 :child (timeline-overview form)))))
                                     (scene2d-construct (scene2d-label :string (format nil "DURATION: ~,3Fs" duration)))
                                     (setf status-label (scene2d-construct (scene2d-label))))
                     :orientation :vertical
                     :alignment (:start :center)))))))
      :end :end)
     status-label)))

(defun edit-animation (timeline &optional bindings)
  (setf bindings (typecase bindings
                   (hash-table (loop :for var :being :each hash-key :of bindings :using (hash-value val)
                                     :collect (cons var (constantly val))))
                   (list (loop :for (var . val) :in bindings :collect (cons var (ensure-function val)))))
        timeline (if timeline timeline (list 'ute:timeline nil)))
  (let ((preview-tween (ute:tween :to ()))
        (preview-container (scene2d-construct (scene2d-container)))
        (status-label nil))
    (flet ((preview (&optional (form timeline))
             (when (second form)
               (setf timeline form
                     (values (eon::scene2d-container-content preview-container) status-label)
                     (timeline-overview-window form)))
             (scene2d-layout preview-container)))
      (with-preview-function #'preview
        (let ((hook (add-game-loop-hook (lambda ()
                                          (when (controller-button-down-p :r3)
                                            (cond
                                              ((controller-button-up-p :l2)
                                               (ute:kill preview-tween))
                                              ((controller-button-down-p :l2)
                                               (unless (ute:startedp preview-tween)
                                                 (when (second timeline)
                                                   (ute:start (setf preview-tween (eval-timeline timeline))))))))
                                          (when status-label
                                            (setf (scene2d-label-string status-label) (if (ute:startedp preview-tween) "PLAYING" "STOPPED"))))
                                        :after t)))
          (setf *eval-timeline-bindings* bindings)
          (async
            (scene2d-group-add-child *debug-window-group* preview-container)
            (preview)
            (with-sent-preview-function ()
              (setf timeline (await (edit-timeline timeline))))
            (scene2d-group-remove-child *debug-window-group* preview-container)
            (remove-game-loop-hook hook)
            timeline))))))

(defun timeline-form-places (form)
  (destructuring-case form
    (((:to :from) (places &rest targets) &rest options)
     (declare (ignore targets options)) (copy-list places))
    (((:sequence :parallel) &rest children)
     (mapcan #'timeline-form-places children))
    ((ute:timeline form) (timeline-form-places form))))

(defun timeline-tweens (form)
  (destructuring-case form
    ((:tween form) (list form))
    (((:sequence :parallel) &rest children)
     (mapcan #'timeline-tweens children))
    ((ute:timeline form) (timeline-tweens form))))

(defun place-tween-accessor-form (place)
  (with-gensyms (value)
    `(lambda (&optional ,value)
       (if ,value (setf ,place ,value) ,place))))

(defun simple-form-variables (form)
  (typecase form
    (cons (mapcan #'simple-form-variables (cdr form)))
    (symbol (list form))))

(defun simple-form-functions (form)
  (typecase form
    (cons (cons (car form) (mapcan #'simple-form-functions (cdr form))))))

(defmacro with-edited-animation (form &optional captures)
  `(progn
     (async
       (print (await ,(if (and (listp form) (eql (car form) 'ute:timeline))
                          (let* ((places (timeline-form-places form))
                                 (tweens (timeline-tweens form))
                                 (place-functions (remove-duplicates (mapcan #'simple-form-functions (copy-list places))))
                                 (nonplace-functions (remove-duplicates (mapcan #'simple-form-functions (copy-list tweens))))
                                 (variables (remove-duplicates (mapcan #'simple-form-variables (append places tweens)))))
                            `(edit-animation ',form (list ,@(loop :for function :in place-functions
                                                                  :unless (fboundp function)
                                                                    :nconc `((cons '(,function) #',function)
                                                                             (cons '(setf ,function) #'(setf ,function))))
                                                          ,@(loop :for function :in nonplace-functions
                                                                  :unless (fboundp function)
                                                                    :collect `(cons '(,function) #',function))
                                                          ,@(loop :for variable :in variables
                                                                  :unless (boundp variable)
                                                                    :collect `(cons ',variable ,(place-tween-accessor-form variable)))
                                                          ,@(loop :for capture :in captures
                                                                  :collect `(cons ',capture
                                                                                  ,(typecase capture
                                                                                     ((cons (eql setf) (cons symbol null)) `(function ,capture))
                                                                                     (list `(function ,(car capture)))
                                                                                     (symbol (place-tween-accessor-form capture))))))))
                          `(edit-animation ',form)))))
     ,form))

(define-standalone-editor animation-editor edit-animation (list 'ute:timeline nil))
