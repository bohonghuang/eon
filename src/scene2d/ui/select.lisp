(in-package #:eon)

(deftype select-box-entry () `scene2d-focusable)

(defun select-box-entry-content (entry)
  (scene2d-focusable-content entry))

(defun (setf select-box-entry-content) (content entry)
  (setf (scene2d-focusable-content entry) content))

(defgeneric select-box-entry-focused-p (entry))

(defgeneric (setf select-box-entry-focused-p) (value entry))

(defstruct select-box-border-entry-style
  (color (raylib:make-color :r 255 :g 97 :b 90 :a 255) :type raylib:color))

(defstruct (select-box-border-entry (:include scene2d-focusable))
  (rectangle (raylib:make-rectangle) :type raylib:rectangle)
  (style (make-select-box-border-entry-style) :type select-box-border-entry-style))

(defun select-box-border-entry (child)
  (make-select-box-border-entry :content child))

(defmethod scene2d-draw ((entry select-box-border-entry) position origin scale rotation tint)
  (declare (ignore origin rotation scale tint))
  (call-next-method)
  (let ((border (select-box-border-entry-rectangle entry)))
    (let ((x (truncate (+ (raylib:vector2-x position) (raylib:rectangle-x border))))
          (y (truncate (+ (raylib:vector2-y position) (raylib:rectangle-y border))))
          (width (truncate (raylib:rectangle-width border)))
          (height (truncate (raylib:rectangle-height border))))
      (clet ((border-color (foreign-alloca '(:struct raylib:color))))
        (raylib:%color-tint border-color (& (select-box-border-entry-style-color (select-box-border-entry-style entry))) (& tint))
        (raylib:%draw-rectangle-lines x y width height border-color)))))

(defconstant +select-box-border-entry-rectangle-padding+ 0.0)

(defmethod scene2d-layout ((entry select-box-border-entry))
  (call-next-method)
  (let ((position (scene2d-node-position (select-box-border-entry-content entry)))
        (size (scene2d-size (select-box-border-entry-content entry)))
        (border (select-box-border-entry-rectangle entry)))
    (setf (raylib:rectangle-x border) (- (raylib:vector2-x position) +select-box-border-entry-rectangle-padding+)
          (raylib:rectangle-y border) (- (raylib:vector2-y position) +select-box-border-entry-rectangle-padding+)
          (raylib:rectangle-width border) (+ (raylib:vector2-x size) (* 2.0 +select-box-border-entry-rectangle-padding+))
          (raylib:rectangle-height border) (+ (raylib:vector2-y size) (* 2.0 +select-box-border-entry-rectangle-padding+)))))

(defmethod select-box-entry-focused-p ((entry select-box-border-entry))
  (plusp (raylib:color-a (select-box-border-entry-style-color (select-box-border-entry-style entry)))))

(defmethod (setf select-box-entry-focused-p) (value (entry select-box-border-entry))
  (setf (raylib:color-a (select-box-border-entry-style-color (select-box-border-entry-style entry))) (if value 255 0))) ; TODO: Avoid modifying the style.

(defstruct select-box-style
  (label-style (make-scene2d-label-style) :type scene2d-label-style)
  (entry-type 'select-box-border-entry :type (or symbol function)))

(defstruct (select-box (:include scene2d-table))
  (dimension 1 :type positive-fixnum)
  (style (make-select-box-style) :type select-box-style))

(defmethod scene2d-layout ((select-box select-box))
  (loop :with alignment := (make-scene2d-alignment :vertical :start :horizontal :start)
        :for box :in (scene2d-box-children select-box)
        :do (loop :for cell :in (scene2d-box-content box)
                  :do (setf (scene2d-cell-alignment cell) alignment)))
  (call-next-method))

(defun select-box-entries (box)
  (mapcan #'identity (scene2d-table-children box)))

(defun select-box-add-child (box child)
  (let* ((alignment (make-scene2d-alignment :vertical :start :horizontal :start))
         (constructor (select-box-style-entry-type (select-box-style box)))
         (entry (funcall constructor child)))
    (when (zerop (rem (length (select-box-entries box)) (select-box-dimension box)))
      (scene2d-table-newline box))
    (setf (scene2d-table-cell-alignment (scene2d-table-add-child box entry)) alignment)
    entry))

(defun select-box-children (box)
  (mapcar #'select-box-entry-content (select-box-entries box)))

(defmethod scene2d-construct-form ((type (eql 'select-box))
                                   &rest
                                     args
                                   &key
                                     (entries)
                                     (children entries)
                                     (style `(make-select-box-style))
                                     (dimensions)
                                   &allow-other-keys)
  (remove-from-plistf args :children :dimensions :entries)
  (setf dimensions (etypecase dimensions
                     (positive-fixnum (list dimensions t))
                     (null (list 1 t))
                     ((cons t (cons t null)) dimensions)))
  (assert (= (length dimensions) 2))
  (multiple-value-bind (orientation dimension)
      (cond
        ((eql (first dimensions) t)
         (values :horizontal (second dimensions)))
        ((eql (second dimensions) t)
         (values :vertical (first dimensions))))
    (check-type dimension positive-fixnum)
    (let ((style-form (scene2d-argument-construct-form style)))
      (with-gensyms (box style)
        `(let* ((,style ,style-form)
                (,box (make-select-box :dimension ,dimension :orientation ,orientation . ,args)))
           (declare (ignorable ,style))
           ,(scene2d-construct-children-form
             (etypecase children
               (list (mapcar
                      (lambda (child)
                        (let ((selection (scene2d-argument-construct-form child)))
                          (if (stringp selection)
                              `(scene2d-construct (scene2d-label :style (select-box-style-label-style ,style)
                                                                 :string ,selection))
                              selection)))
                      children))
               (symbol children))
             (lambda (child) `(select-box-add-child ,box ,child)))
           ,box)))))

(define-scene2d-default-construct-form select-box-style (label-style entry-type))

(defun select-box-promise-index (box &optional (initial-index 0) (handler (constantly nil)))
  (let* ((entries (select-box-entries box))
         (initial-focused (nth initial-index entries))
         (manager (make-scene2d-focus-manager :focusables (cons initial-focused (remove initial-focused entries)))))
    (setf (select-box-entry-focused-p initial-focused) t)
    (mapc (curry #'(setf select-box-entry-focused-p) nil) (remove initial-focused entries))
    (async
      (loop
        (funcall handler manager)
        (let ((key (await (promise-pressed-key))))
          (case key
            ((:up :down :left :right)
             (setf (select-box-entry-focused-p (scene2d-focus-manager-focused manager)) nil)
             (scene2d-focus-manager-handle-key manager key))
            ((:a) (return (position (scene2d-focus-manager-focused manager) entries)))
            ((:b) (return nil)))
          (when-let ((result (funcall handler manager key)))
            (etypecase result
              ((eql t) (return nil))
              (non-negative-fixnum (return result))))
          (setf (select-box-entry-focused-p (scene2d-focus-manager-focused manager)) t))))))

(defstruct (table-select-box (:include select-box))
  (table (make-scene2d-table) :type scene2d-table))

(defmethod scene2d-layout ((select-box table-select-box))
  (scene2d-layout (table-select-box-table select-box))
  (call-next-method))

(defun table-select-box (table)
  (let ((select-box (make-table-select-box :table table :orientation (scene2d-table-orientation table))))
    (mapc (curry #'select-box-add-child select-box) (scene2d-box-children table)) select-box))

(defun swappable-select-box (box)
  (loop :with constructor := (select-box-style-entry-type (select-box-style box))
        :for entry :in (select-box-entries box)
        :for content := (funcall constructor (select-box-entry-content entry))
        :do (setf (select-box-entry-focused-p content) nil
                  (select-box-entry-content entry) content)
        :finally (return box)))

(defun swappable-select-box-promise-index (box &optional (initial-index 0) (handler (constantly nil)))
  (let ((entries (select-box-entries box))
        (swap-entries (mapcar #'select-box-entry-content (select-box-entries box)))
        (swap-index nil))
    (let* ((promise (promise:make))
           (handler (lambda (manager &optional key)
                      (if key
                          (progn
                            (case key
                              (:select (let ((index (position (scene2d-focus-manager-focused manager) entries)))
                                         (when swap-index
                                           (setf (select-box-entry-focused-p (nth swap-index swap-entries)) nil)
                                           (promise:succeed promise (cons index swap-index)))
                                         (setf swap-index (if (eql index swap-index) nil index))
                                         (when swap-index
                                           (setf (select-box-entry-focused-p (nth swap-index swap-entries)) t)))))
                            (funcall handler manager key))
                          (funcall handler manager)))))
      (async
        (loop
          (let ((index (await (aselect (select-box-promise-index box initial-index handler) promise)))) ; TODO: Handle the promise leaked here.
            (when (consp index)
              (return index))
            (when swap-index
              (setf (select-box-entry-focused-p (nth swap-index swap-entries)) nil))
            (if index
                (if swap-index
                    (if (= index swap-index)
                        (setf swap-index nil)
                        (return (cons index swap-index)))
                    (return index))
                (if swap-index
                    (setf swap-index nil)
                    (return nil)))
            (when swap-index
              (setf (select-box-entry-focused-p (nth swap-index swap-entries)) t))))))))

(defstruct (select-box-transparent-entry (:include scene2d-focusable)))

(defun select-box-transparent-entry (content)
  (make-select-box-transparent-entry :content content))

(defmethod select-box-entry-focused-p ((entry select-box-transparent-entry))
  (select-box-entry-focused-p (select-box-transparent-entry-content entry)))

(defmethod (setf select-box-entry-focused-p) (value (entry select-box-transparent-entry))
  (setf (select-box-entry-focused-p (select-box-transparent-entry-content entry)) value))

(defmethod select-box-entry-focused-p ((constructed scene2d-constructed))
  (when-let ((background-focused (gethash :background-focused (scene2d-constructed-metadata constructed))))
    (plusp (raylib:color-a (scene2d-color background-focused)))))

(defmethod (setf select-box-entry-focused-p) (value (constructed scene2d-constructed))
  (when-let ((background-focused (gethash :background-focused (scene2d-constructed-metadata constructed)))
             (background-unfocused (or (gethash :background (scene2d-constructed-metadata constructed))
                                       (gethash :background-unfocused (scene2d-constructed-metadata constructed)))))
    (setf (raylib:color-a (scene2d-color background-focused)) (if value 255 0) (raylib:color-a (scene2d-color background-unfocused)) (if value 0 255))))
