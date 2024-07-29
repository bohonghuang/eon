(in-package #:eon.editor)

(defun type-construct-arguments (type)
  (let ((specialized-args (loop :for method :in (c2mop:generic-function-methods #'scene2d-construct-form)
                                :for specializer := (first (c2mop:method-specializers method))
                                :when (and (typep specializer 'c2mop:eql-specializer) (eql (c2mop:eql-specializer-object specializer) type))
                                  :return (mapcar #'caar (nth-value 3 (parse-ordinary-lambda-list (c2mop:method-lambda-list method))))))
        (common-args (when (subtypep type 'scene2d-node)
                       '(:position :origin :scale :color :rotation :name))))
    (nconc specialized-args common-args)))

(defun plist-props-alist (plist props)
  (loop :for prop :in props
        :collect (cons prop (getf plist prop :unspecified))))

(defgeneric edit-construct-form (type &rest args))

(defparameter *construct-widgets* '(scene2d-group
                                    scene2d-box
                                    scene2d-window
                                    scene2d-image
                                    scene2d-cell
                                    scene2d-label
                                    scene2d-scissor
                                    scene2d-scroll-region
                                    scene2d-margin
                                    scene2d-container
                                    scene2d-coordinate-truncator
                                    scene2d-tile-scroll-region
                                    select-box
                                    dialog-box
                                    dialog-box-text))

(defun edit-construct (form)
  (if form (apply #'edit-construct-form form)
      (with-received-preview-function
        (async
          (let ((type (await (promise-selection "Select a widget type." *construct-widgets*))))
            (with-sent-preview-function ()
              (when type (await (edit-construct-form type)))))))))

(defgeneric edit-construct-argument (type name &optional value))

(defmethod edit-construct-form (type &rest args)
  #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (with-received-preview-function
    (async
      (do-non-nil (argument (progn (preview `(debug-container :child ,(cons type args)))
                                   (await (promise-selection (format nil "Edit ~A~@[ (aka ~A)~]." type (getf args :name))
                                                             (plist-props-alist args (type-construct-arguments type)))))
                            (cons type args))
        (with-sent-preview-function ((cons type args) (getf args argument))
          (if (controller-button-down-p :l3) (remf args argument)
              (with-specified-value (value (await (apply #'edit-construct-argument type argument
                                                         (with-specified-value (arg (getf args argument :unspecified))
                                                           (list arg))))
                                           (remf args argument))
                (setf (getf args argument) value))))))))

(defmethod edit-construct-form :around (type &rest args)
  (with-received-preview-function
    (let (operations)
      (push (cons 'edit (lambda (form) (declare (ignore form)) (with-sent-preview-function () (call-next-method)))) operations)
      (when (subtypep type 'scene2d-node)
        (push (cons 'wrap (lambda (form)
                            (async
                              (let ((parent-type (await (promise-selection "Select the parent."
                                                                           (remove-if-not
                                                                            (lambda (type)
                                                                              (or (member :child (type-construct-arguments type))
                                                                                  (member :children (type-construct-arguments type))))
                                                                            *construct-widgets*)))))
                                (cond
                                  ((member :child (type-construct-arguments parent-type))
                                   (list parent-type :child (cons type args)))
                                  ((member :children (type-construct-arguments parent-type))
                                   (list parent-type :children (list (cons type args))))
                                  (t form))))))
              operations))
      (when-let ((child (or (getf args :child) (getf args :children))))
        (push (cons 'unwrap (lambda (form)
                              (declare (ignore form))
                              (async
                                (if (symbolp (car child)) child (first child)))))
              operations))
      (nreversef operations)
      (if (or (= (length operations) 1) (not (controller-button-down-p :l2)))
          (funcall (cdr (first operations)) (cons type args))
          (async
            (await (funcall (or (assoc-value operations (await (promise-selection
                                                                (format nil "Select an operation for ~A." type)
                                                                (mapcar #'car operations))))
                                (constantly (async (cons type args))))
                            (cons type args))))))))

(defun edit-expression (&optional value)
  (async
    (read-from-string
     (non-empty-or
         (nstring-upcase
          (await (promise-input-text "Enter an expression." (prin1-to-string value))))
         (string nil)))))

(defun edit-string (&optional value)
  (async
    (non-empty-or
        (await (promise-input-text "Enter an expression." value))
        nil)))

(defmethod edit-construct-argument :around (type name &optional (value nil valuep))
  (when (controller-button-down-p :r3) (setf value (list 'scene2d-construct nil)))
  (if (typep value '(cons (eql scene2d-construct) (cons symbol null)))
      (async
        (let ((symbol (await (edit-expression (second value)))))
          (if symbol (progn (setf (second value) symbol) value) (specified-value value valuep))))
      (call-next-method)))

(defmethod edit-construct-argument (type (name (eql :name)) &optional (value nil valuep))
  (async
    (or (await (edit-expression value))
        (specified-value value valuep))))

(defmethod edit-construct-argument (type (name (eql :text)) &optional (value nil valuep))
  (async
    (or (await (edit-string (or value "")))
        (specified-value value valuep))))

(defmethod edit-construct-argument (type (name (eql :string)) &optional (value "" valuep))
  (async
    (non-empty-or
        (await (promise-input-text "Enter text." value))
        (specified-value value valuep))))

(defmethod edit-construct-argument (type (name (eql :drawable)) &optional (value nil valuep))
  (async
    (or (await (edit-asset value '(raylib:texture)))
        (specified-value value valuep))))

(defmethod edit-construct-argument (type (name (eql :tile)) &optional (value nil valuep))
  (async
    (or (await (edit-asset value '(raylib:texture)))
        (specified-value value valuep))))

(defmethod edit-construct-argument (type (name (eql :background)) &optional (value (list 'n-patch) valuep))
  (async
    (or (await (edit-construct value))
        (specified-value value valuep))))

(defmethod edit-construct-argument (type (name (eql :body)) &optional (value (list 1.0 1.0 -1.0 -1.0) valuep))
  (async
    (or (await (edit-rectangle value))
        (specified-value value valuep))))

(defmethod edit-construct-argument (type (name (eql :texture)) &optional (value nil valuep))
  (async
    (or (await (edit-asset value '(raylib:texture)))
        (specified-value value valuep))))

(defmethod edit-construct-argument (type (name (eql :font)) &optional (value nil valuep))
  (async
    (or (await (edit-asset value '(raylib:font)))
        (specified-value value valuep))))

(defmethod edit-construct-argument ((type (eql 'text-style)) (name (eql :size)) &optional (value 15.0 valuep))
  (with-received-preview-function
    (async (or (with-sent-preview-function ()
                 (await (edit-float value)))
               (specified-value value valuep)))))

(defmethod edit-construct-argument (type (name (eql :spacing)) &optional (value 1.0 valuep))
  (with-received-preview-function
    (async (or (with-sent-preview-function ()
                 (await (edit-float value)))
               (specified-value value valuep)))))

(defmethod edit-construct-argument (type (name (eql :text-style)) &optional (value (list 'text-style)))
  (with-received-preview-function
    (async
      (with-sent-preview-function ()
        (await (edit-construct value))))))

(defmethod edit-construct-argument (type (name (eql :label-style)) &optional (value (list 'scene2d-label-style)))
  (with-received-preview-function
    (async
      (with-sent-preview-function ()
        (await (edit-construct value))))))

(defmethod edit-construct-argument (type (name (eql :tile-scroll-style)) &optional (value (list 'scene2d-tile-scroll-style)))
  (with-received-preview-function
    (async
      (with-sent-preview-function ()
        (await (edit-construct value))))))

(defun type-style-class-name (type)
  (when-let ((style-class (find-class (let ((*package* (symbol-package type))) (symbolicate type '#:-style)) nil)))
    (class-name style-class)))

(defmethod edit-construct-argument (type (name (eql :style)) &optional (value
                                                                        (list (or (type-style-class-name type)
                                                                                  (no-applicable-method
                                                                                   #'edit-construct-argument
                                                                                   type name nil)))))
  (with-received-preview-function
    (async
      (with-sent-preview-function ()
        (await (edit-construct value))))))

(defmethod edit-construct-argument (type (name (eql :position)) &optional (value (list 0.0 0.0) valuep))
  (with-received-preview-function
    (async (or (with-sent-preview-function ()
                 (await (edit-vector2 value)))
               (specified-value value valuep)))))

(defmethod edit-construct-argument (type (name (eql :origin)) &optional (value (list 0.0 0.0) valuep))
  (with-received-preview-function
    (async (or (with-sent-preview-function ()
                 (await (edit-vector2 value)))
               (specified-value value valuep)))))

(defmethod edit-construct-argument (type (name (eql :scale)) &optional (value (list 1.0 1.0) valuep))
  (with-received-preview-function
    (async (or (with-sent-preview-function ()
                 (await (edit-vector2 value)))
               (specified-value value valuep)))))

(defmethod edit-construct-argument (type (name (eql :size)) &optional (value (list 0.0 0.0) valuep))
  (with-received-preview-function
    (async (or (with-sent-preview-function ()
                 (await (edit-vector2 value '(width height))))
               (specified-value value valuep)))))

(defmethod edit-construct-argument (type (name (eql :rotation)) &optional (value 0.0 valuep))
  (with-received-preview-function
    (async (or (with-sent-preview-function ()
                 (await (edit-float value)))
               (specified-value value valuep)))))

(defmethod edit-construct-argument (type (name (eql :child)) &optional (value nil valuep))
  (with-received-preview-function
    (async (or (with-sent-preview-function ()
                 (await (edit-construct value)))
               (specified-value value valuep)))))

(defmethod edit-construct-argument (type (name (eql :top)) &optional (value 0.0 valuep))
  (with-received-preview-function
    (async (or (with-sent-preview-function ()
                 (await (edit-float value)))
               (specified-value value valuep)))))

(defmethod edit-construct-argument (type (name (eql :bottom)) &optional (value 0.0 valuep))
  (with-received-preview-function
    (async (or (with-sent-preview-function ()
                 (await (edit-float value)))
               (specified-value value valuep)))))

(defmethod edit-construct-argument (type (name (eql :left)) &optional (value 0.0 valuep))
  (with-received-preview-function
    (async (or (with-sent-preview-function ()
                 (await (edit-float value)))
               (specified-value value valuep)))))

(defmethod edit-construct-argument (type (name (eql :right)) &optional (value 0.0 valuep))
  (with-received-preview-function
    (async (or (with-sent-preview-function ()
                 (await (edit-float value)))
               (specified-value value valuep)))))

(defmethod edit-construct-argument (type (name (eql :orientation)) &optional (value :vertical valuep))
  (async (or (await (promise-selection "Select orientation." '(:vertical :horizontal)))
             (specified-value value valuep))))

(defun promise-alignment ()
  (promise-selection "Select alignment." '(:start :center :end)))

(defmethod edit-construct-argument ((type (eql 'scene2d-box)) (name (eql :alignment)) &optional (value :center valuep))
  (async (or (await (promise-alignment)) (specified-value value valuep))))

(defmethod edit-construct-argument (type (name (eql :alignment)) &optional (value (list :center :center)))
  (with-received-preview-function
    (async
      (selection-case "Select alignment type."
        (:horizontal
         (setf (first value) (await (promise-alignment)))
         (preview value))
        (:vertical
         (setf (second value) (await (promise-alignment)))
         (preview value)))
      value)))

(defmethod edit-construct-argument ((type (eql 'scene2d-label-style)) (name (eql :shadow)) &optional (value 'raylib:+gray+ valuep))
  (async (or (await (edit-construct-argument type :color value)) (specified-value value valuep))))

(defmethod edit-construct-argument ((type (eql 'scene2d-label-style)) (name (eql :border)) &optional (value 'raylib:+gray+ valuep))
  (async (or (await (edit-construct-argument type :color value)) (specified-value value valuep))))

(defmethod edit-construct-argument (type (name (eql :color)) &optional (value 'raylib:+white+ valuep))
  (with-received-preview-function
    (async (or (await (promise-selection
                       "Select a color."
                       '(raylib:+lightgray+
                         raylib:+gray+
                         raylib:+darkgray+
                         raylib:+yellow+
                         raylib:+gold+
                         raylib:+orange+
                         raylib:+pink+
                         raylib:+red+
                         raylib:+maroon+
                         raylib:+green+
                         raylib:+lime+
                         raylib:+darkgreen+
                         raylib:+skyblue+
                         raylib:+blue+
                         raylib:+darkblue+
                         raylib:+purple+
                         raylib:+violet+
                         raylib:+darkpurple+
                         raylib:+beige+
                         raylib:+brown+
                         raylib:+darkbrown+
                         raylib:+white+
                         raylib:+black+
                         raylib:+blank+
                         raylib:+magenta+
                         raylib:+raywhite+)))
               (specified-value value valuep)))))

(defun edit-children (edit-function &optional children (allow-modification-p t))
  (with-received-preview-function
    (async
      (do-non-nil (selection
                   (await (promise-selection
                           "Edit children."
                           (append (preview children)
                                   (when allow-modification-p '(new)))
                           nil t))
                   children)
        (case selection
          (new
           (nconcf children (list nil))
           (with-sent-preview-function (children (lastcar children))
             (if-let ((construct (await (funcall edit-function (lastcar children)))))
               (setf (lastcar children) construct)
               (setf children (nbutlast children)))))
          (t
           (if-let ((index (position selection children)))
             (if (and (controller-button-down-p :l3) allow-modification-p)
                 (deletef children selection)
                 (with-sent-preview-function (children (nth index children))
                   (setf (nth index children) (await (funcall edit-function selection)))))
             (when-let ((index-a (position (car selection) children))
                        (index-b (position (cdr selection) children)))
               (rotatef (nth index-a children) (nth index-b children))))))))))

(defmethod edit-construct-argument (type (name (eql :children)) &optional (value (list) valuep))
  (with-received-preview-function
    (async
      (or (with-sent-preview-function () (await (edit-children #'edit-construct value)))
          (or value (specified-value value valuep))))))

(defmethod edit-construct-argument (type (name (eql :lines-spacing)) &optional (value 0.0 valuep))
  (with-received-preview-function
    (async (or (with-sent-preview-function ()
                 (await (edit-float value)))
               (specified-value value valuep)))))

(defmethod edit-construct-argument (type (name (eql :indicator)) &optional (value nil valuep))
  (async
    (or (await (edit-asset value '(raylib:texture)))
        (specified-value value valuep))))

(defun edit-ui (construct)
  (let ((preview-container (scene2d-construct (scene2d-container))))
    (flet ((preview (&optional (form construct))
             (setf construct form
                   (eon::scene2d-container-content preview-container)
                   (when-let ((cell (ignore-errors
                                     (screen-cell
                                      (eval `(scene2d-construct ,form))
                                      :center :center))))
                     (scene2d-layout cell) cell))))
      (with-preview-function #'preview
        (async
          (scene2d-group-add-child *debug-window-group* preview-container)
          (preview)
          (with-sent-preview-function ()
            (setf construct (await (edit-construct construct))))
          (scene2d-group-remove-child *debug-window-group* preview-container)
          construct)))))

(define-standalone-editor ui-editor)
