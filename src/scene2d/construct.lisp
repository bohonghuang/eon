(in-package #:eon)

(defgeneric scene2d-construct-argument-form (type arg form)
  (:method (type arg form) (declare (ignore type arg)) form))

(defgeneric scene2d-construct-form (type &rest args)
  (:method (type &rest args)
    (if (find-if (lambda (method &aux (specializer (first (c2mop:method-specializers method))))
                   (when (typep specializer 'c2mop:eql-specializer)
                     (eql (c2mop:eql-specializer-object specializer) type)))
                 (c2mop:generic-function-methods #'load-asset))
        `(load-asset ',type . ,args)
        (apply #'no-applicable-method #'scene2d-construct-form type args)))
  (:method :around ((type symbol) &rest args)
    (declare (special scene2d-construct-hash-table))
    (let (name)
      (ignore-errors
       (setf name (prog1 (getf args :name) (remove-from-plistf args :name))
             args (loop :for (arg form) :on args :by #'cddr
                        :nconc (list arg (scene2d-construct-argument-form type arg form)))))
      (let ((result (apply #'call-next-method type (mapcar #'scene2d-argument-construct-form args))))
        (if name
            (with-gensyms (form)
              `(let ((,form ,result))
                 (setf (gethash ',name ,scene2d-construct-hash-table) ,form) ,form))
            result)))))

(defun scene2d-argument-construct-form (arg)
  (typecase arg
    (null arg)
    (proper-list
     (if-let ((form (ignore-errors (apply #'scene2d-construct-form arg))))
       form arg))
    (t arg)))

(defmacro scene2d-construct (root)
  (with-gensyms (scene2d-construct-hash-table)
    (declare (special scene2d-construct-hash-table))
    `(let ((,scene2d-construct-hash-table (make-hash-table)))
       (values ,(apply #'scene2d-construct-form
                       (etypecase root
                         (list root)
                         (symbol (cond
                                   ((boundp root) (symbol-value root))
                                   ((compute-applicable-methods #'scene2d-construct-form (list root)) (list root))
                                   (t (error "Cannot construct anything from symbol ~A." root))))))
               ,scene2d-construct-hash-table))))

(defmacro define-scene2d-default-construct-form (type (&rest args))
  `(defmethod scene2d-construct-form ((type (eql ',type)) &rest args &key ,@args &allow-other-keys)
     (declare (ignore . ,args))
     (when-let ((child (getf args :child)))
       (remove-from-plistf args :child)
       (setf args (nconc `(:content ,child) args)))
     `(,',(symbolicate '#:make- type) . ,args)))

(defmacro define-scene2d-default-vector2-argument-form (arg)
  `(defmethod scene2d-construct-argument-form (type (arg (eql ,arg)) form)
     (if (and (listp form) (= (length form) 2) (every #'constantp form))
         `(raylib:make-vector2 :x ,(coerce (eval (first form)) 'single-float)
                               :y ,(coerce (eval (second form)) 'single-float))
         form)))

(defmacro define-scene2d-default-rectangle-argument-form (arg)
  `(defmethod scene2d-construct-argument-form (type (arg (eql ,arg)) form)
     (if (and (listp form) (= (length form) 4) (every #'constantp form))
         `(raylib:make-rectangle :x ,(coerce (eval (first form)) 'single-float)
                                 :y ,(coerce (eval (second form)) 'single-float)
                                 :width ,(coerce (eval (third form)) 'single-float)
                                 :height ,(coerce (eval (fourth form)) 'single-float))
         form)))

(defmethod scene2d-construct-argument-form (type (arg (eql :alignment)) form)
  (typecase form
    ((cons keyword t)
     (destructuring-bind (horizontal &optional vertical) form
       `(make-scene2d-alignment :horizontal ,horizontal :vertical ,(or vertical horizontal))))
    (t form)))

(defmethod scene2d-construct-argument-form (type (arg (eql :color)) form)
  (etypecase form
    ((cons (unsigned-byte 8) (cons (unsigned-byte 8) (cons (unsigned-byte 8) (cons (unsigned-byte 8) null))))
     (destructuring-bind (r g b a) form
       `(raylib:make-color :r ,r :g ,g :b ,b :a ,a)))
    (t `(raylib:copy-color ,form))))

(define-scene2d-default-vector2-argument-form :size)

(define-scene2d-default-vector2-argument-form :scale)

(define-scene2d-default-vector2-argument-form :origin)

(define-scene2d-default-vector2-argument-form :position)

(define-scene2d-default-construct-form scene2d-container (child))

(define-scene2d-default-construct-form scene2d-margin (child left right top bottom))

(define-scene2d-default-construct-form scene2d-cell (child size alignment))

(define-scene2d-default-construct-form scene2d-max-cell (child size alignment))

(define-scene2d-default-construct-form scene2d-table ())

(define-scene2d-default-construct-form scene2d-coordinate-truncator (child))

(define-scene2d-default-construct-form scene2d-scissor (child size))

(defun scene2d-construct-children-form (children-form add-child-form)
  (with-gensyms (child)
    (etypecase children-form
      ((or (and symbol (not null)) (cons symbol list))
       `(loop :for ,child :in ,children-form
              :do ,(funcall add-child-form child)))
      (list `(progn ,@(mapcar (compose add-child-form #'scene2d-argument-construct-form) children-form))))))

(defmethod scene2d-construct-form ((type (eql 'scene2d-box)) &rest args &key children orientation (alignment `(make-scene2d-alignment)) &allow-other-keys)
  (declare (ignore orientation))
  (remove-from-plistf args :children)
  (remove-from-plistf args :alignment)
  (with-gensyms (box align)
    `(let ((,box (make-scene2d-box . ,args))
           (,align ,alignment))
       (declare (ignorable ,align))
       ,(scene2d-construct-children-form children (lambda (child) `(setf (scene2d-cell-alignment (scene2d-box-add-child ,box ,child)) ,align)))
       ,box)))

(defmethod scene2d-construct-form ((type (eql 'scene2d-window)) &rest args &key child (style `(make-scene2d-window-style)) &allow-other-keys)
  (remove-from-plistf args :child :style)
  `(make-scene2d-window :content (list (ensure-scene2d-node (scene2d-window-style-background ,style)) ,child) . ,args))

(defmethod scene2d-construct-form ((type (eql 'scene2d-flow-box)) &rest args &key children &allow-other-keys)
  (remove-from-plistf args :children)
  (with-gensyms (box)
    `(let ((,box (make-scene2d-flow-box . ,args)))
       ,(scene2d-construct-children-form children (lambda (child) `(scene2d-flow-box-add-child ,box ,child)))
       ,box)))

(define-scene2d-default-construct-form scene2d-image ())

(defmethod scene2d-construct-form ((type (eql 'scene2d-image)) &rest args &key drawable &allow-other-keys)
  (remove-from-plistf args :drawable)
  `(make-scene2d-image ,@(when drawable `(:content ,(scene2d-argument-construct-form drawable))) ,@args))

(define-scene2d-default-construct-form scene2d-nine-patch ())

(defmethod scene2d-construct-form ((type (eql 'scene2d-group)) &rest args &key children &allow-other-keys)
  (remove-from-plistf args :children)
  (with-gensyms (group)
    `(let ((,group (make-scene2d-group . ,args)))
       ,(scene2d-construct-children-form children (lambda (child) `(scene2d-group-add-child ,group ,child)))
       ,group)))

(defmethod scene2d-construct-form ((type (eql 'scene2d-label)) &rest args &key (string "") (style `(make-scene2d-label-style)) &allow-other-keys)
  (remove-from-plistf args :string :style)
  (let ((style-form (scene2d-argument-construct-form style)))
    (with-gensyms (style)
      `(let ((,style ,style-form))
         (make-scene2d-label :content (make-text :string ,string :style (scene2d-label-style-text-style ,style)) :style ,style . ,args)))))

(define-scene2d-default-construct-form scene2d-shaderable-container (child shader))

(define-scene2d-default-construct-form scene2d-canvas (size renderer))

(define-scene2d-default-construct-form scene2d-tween-container ())

(define-scene2d-default-construct-form n-patch (texture body layout))

(define-scene2d-default-rectangle-argument-form :body)

(define-scene2d-default-construct-form text-style (font size spacing))

(define-scene2d-default-construct-form scene2d-label-style (text-style color shadow outline))

(define-scene2d-default-construct-form scene2d-window-style (background))

(define-scene2d-default-construct-form scene2d-tiling-scroll-region (size style))

(define-scene2d-default-construct-form scene2d-tiling-scroll-style (tile))

(define-scene2d-default-construct-form scene2d-tiling-scroll-region-style (tiling-scroll-style))

(defstruct (scene2d-constructed (:include scene2d-container))
  (metadata (make-hash-table) :type hash-table))

(defmacro define-scene2d-constructed (name construct-form &rest options)
  (labels ((collect-names (form)
             (when (consp form)
               (typecase (car form)
                 ((eql :name) (cons (cadr form) (collect-names (cddr form))))
                 (cons (nconc (collect-names (car form)) (collect-names (cdr form))))
                 (t (collect-names (cdr form)))))))
    (destructuring-bind (class &key (constructor (find-symbol (format nil "~A-~A" '#:make class) (symbol-package class))))
        (or (cdr (find :class options :key #'car)) '(scene2d-constructed :constructor make-scene2d-constructed))
      (let ((children (collect-names construct-form))
            (constructor (cdr (find :constructor options :key #'car)))
            (class-constructor constructor)
            (methods (remove :animation options :key #'car :test-not #'eq))
            (conc-name (car (or (cdr (find :conc-name options :key #'car)) (list name))))
            (accessors nil))
        (with-gensyms (root table)
          `(progn
             (defun ,(if constructor (symbolicate '#:%make- name) (symbolicate '#:make- name)) ()
               (multiple-value-bind (,root ,table)
                   (scene2d-construct ,construct-form)
                 (setf (gethash ',name ,table) (,class-constructor :content ,root :metadata ,table))))
             ,@(when conc-name
                 (loop :for child :in children
                       :nconc (loop :for i :from 0
                                    :for child-name := (symbol-name child) :then trimmed-name
                                    :for trimmed-name := (let ((suffix-start (- (length child-name) (length (string '#:-container)))))
                                                           (if (eql (search (string '#:-container) child-name :from-end t) suffix-start)
                                                               (subseq child-name 0 suffix-start) child-name))
                                    :for accessor := (intern (format nil "~A-~A" conc-name child-name) (symbol-package name))
                                    :collect `(defun ,accessor (,name)
                                                ,(loop :for form := `(gethash ',child (scene2d-constructed-metadata ,name))
                                                         :then `(scene2d-container-content ,form)
                                                       :repeat i :finally (return form)))
                                    :do (push (list (intern child-name (symbol-package name)) accessor) accessors)
                                    :until (or (emptyp trimmed-name) (string= trimmed-name child-name)))))
             ,(when constructor
                `(defun ,(symbolicate '#:make- name) . ,constructor))
             ,@(loop :for (method-name args . body) :in (mapcar #'cdr methods)
                     :collect `(defun ,(symbolicate name '#:- method-name) (,name . ,args)
                                 (with-accessors ,accessors ,name . ,body)))
             (eval-when (:compile-toplevel :load-toplevel :execute)
               (define-scene2d-default-construct-form ,name ,(mapcar #'cadar (nth-value 3 (parse-ordinary-lambda-list (car constructor))))))))))))
