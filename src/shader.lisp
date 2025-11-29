(in-package #:eon)

(defgeneric update-shader-uniforms (uniforms shader)
  (:documentation "Update UNIFORMS for SHADER."))

(defmacro define-shaderable-uniforms (name &body slots)
  "Define a shader uniform structure for type NAME, whose following DEFSTRUCT-style slot readers are expected to exist:
- SHADER: The shader of the shader uniforms being defined.
- SHADER-UNIFORMS: The shader uniform structure being defined.

The syntax for defining SLOTS is similar to DEFSTRUCT, but each slot can only be one of the following types:
- SINGLE-FLOAT
- (SIGNED-BYTE 32)
- (UNSIGNED-BYTE 32)
- BOOLEAN
- RAYLIB:VECTOR2
- RAYLIB:VECTOR3
- RAYLIB:VECTOR4
- RAYLIB:COLOR
- RAYLIB:TEXTURE
- RAYLIB:MATRIX

This macro will generate a structure named [NAME]-SHADER-UNIFORMS, as well as an updater named UPDATE-[NAME]-SHADER-UNIFORMS, which should be called on the object after calling RAYLIB:BEGIN-SHADER-MODE."
  (let* ((struct-name (symbolicate name '#:-shader-uniforms))
         (constructor-name (symbolicate '#:make- struct-name))
         (internal-constructor-name (symbolicate '#:%make- struct-name))
         (internal-initializer-name (symbolicate '#:%initialize- struct-name))
         (initializedp-slot-name '#:initializedp)
         (uniform-names) (slot-names) (slot-types) (slot-ctypes) (initial-values))
    (loop :for slot :in slots
          :do (destructuring-bind (slot-name initial-value &key type) slot
                (push slot-name uniform-names)
                (push (translate-camelcase-name slot-name) slot-names)
                (push type slot-types)
                (push (eswitch (type :test #'equal)
                        ('single-float :float)
                        ('(signed-byte 32) :int32)
                        ('(unsigned-byte 32) :uint32)
                        ('boolean :bool)
                        ('raylib:vector2 '(:struct raylib:vector2))
                        ('raylib:vector3 '(:struct raylib:vector3))
                        ('raylib:vector4 '(:struct raylib:vector4))
                        ('raylib:color '(:struct raylib:color))
                        ('raylib:texture '(:struct raylib:texture))
                        ('raylib:matrix '(:struct raylib:matrix)))
                      slot-ctypes)
                (push initial-value initial-values))
          :finally
             (nreversef uniform-names)
             (nreversef slot-names)
             (nreversef slot-types)
             (nreversef slot-ctypes)
             (nreversef initial-values))
    (flet ((slot-name-location (slot-name)
             (symbolicate slot-name '#:-location)))
      (with-gensyms (value instance struct-instance shader temp args shader-supplied-p)
        `(progn
           (defcstruct ,struct-name
             ,@(mapcar #'list slot-names slot-ctypes)
             (,initializedp-slot-name :bool)
             ,@(mapcar (compose (rcurry #'list :int) #'slot-name-location) slot-names))
           (cobj:define-cobject-class (:struct ,struct-name) (:constructor ,internal-constructor-name))
           ,@(loop :for slot-name :in slot-names
                   :for slot-ctype :in slot-ctypes
                   :for slot-accessor := (symbolicate name '#:- slot-name)
                   :collect `(defun ,slot-accessor (,instance)
                               (,(symbolicate struct-name '#:- slot-name) (,struct-name ,instance)))
                   :when (member slot-ctype '(:float :int32 :uint32 :bool))
                     :collect `(defun (setf ,slot-accessor) (,value ,instance)
                                 (setf (,(symbolicate struct-name '#:- slot-name) (,struct-name ,instance)) ,value)))
           ,(let ((initializer-name (symbolicate '#:initialize- struct-name))
                  (updater-name (symbolicate '#:update- struct-name)))
              `(progn
                 (defun ,internal-initializer-name (,struct-instance &key . ,(mapcar #'list slot-names initial-values))
                   (clet ((,instance (cthe (:pointer (:struct ,struct-name)) (& ,struct-instance))))
                     ,@(loop :for slot-name :in slot-names
                             :for slot-type :in slot-types
                             :for slot-ctype :in slot-ctypes
                             :if (member slot-type '((signed-byte 32) (unsigned-byte 32) single-float boolean) :test #'equal)
                               :collect `(setf (-> ,instance ,slot-name) ,slot-name)
                             :else
                               :collect `(let ((,temp ,slot-name))
                                           (csetf (-> ,instance ,slot-name) ([] (cthe (:pointer ,slot-ctype) (& ,temp))))
                                           ,(when (eq slot-type 'raylib:texture)
                                              `(tg:finalize
                                                ,struct-instance
                                                (unload-asset-finalizer ,temp)))))
                     (setf (-> ,instance ,initializedp-slot-name) nil)))
                 (defun ,initializer-name (,instance &optional (,shader (,(symbolicate name '#:-shader) ,instance) ,shader-supplied-p))
                   (let ((,struct-instance (if ,shader-supplied-p ,instance (,struct-name ,instance))))
                     (clet ((,shader (cthe (:pointer (:struct raylib:shader)) (& ,shader)))
                            (,instance (cthe (:pointer (:struct ,struct-name)) (& ,struct-instance))))
                       ,@(loop :for slot-name :in slot-names
                               :for uniform-name :in uniform-names
                               :collect `(setf (-> ,instance ,(slot-name-location slot-name))
                                               (raylib:%get-shader-location ,shader ,uniform-name)))
                       (setf (-> ,instance ,initializedp-slot-name) t))))
                 (defun ,constructor-name (&rest ,args)
                   (declare (dynamic-extent ,args))
                   (let ((,instance (,internal-constructor-name)))
                     (apply #',internal-initializer-name ,instance ,args)
                     ,instance))
                 (defun ,updater-name (,instance &optional (,shader (,(symbolicate name '#:-shader) ,instance) ,shader-supplied-p))
                   (let ((,struct-instance (if ,shader-supplied-p ,instance (,struct-name ,instance))))
                     (clet ((,struct-instance (cthe (:pointer (:struct ,struct-name)) (& ,struct-instance))))
                       (unless (-> ,struct-instance ,initializedp-slot-name)
                         (if ,shader-supplied-p (,initializer-name ,instance ,shader) (,initializer-name ,instance)))
                       (let ((,shader (cthe (:pointer (:struct raylib:shader)) (& ,shader))))
                         ,@(loop :for slot-name :in slot-names
                                 :for slot-type :in slot-types
                                 :for generic-form := `(raylib:%set-shader-value
                                                        ,shader (-> ,struct-instance ,(slot-name-location slot-name))
                                                        ,(case slot-type
                                                           ((raylib:color boolean) temp)
                                                           (t `(& (-> ,struct-instance ,slot-name))))
                                                        ,(foreign-enum-value
                                                          'raylib:shader-uniform-data-type
                                                          (eswitch (slot-type :test #'equal)
                                                            ('(signed-byte 32) :int)
                                                            ('(unsigned-byte 32) :int)
                                                            ('boolean :int)
                                                            ('single-float :float)
                                                            ('raylib:vector2 :vec2)
                                                            ('raylib:vector3 :vec3)
                                                            ('raylib:color :vec4)
                                                            ('raylib:vector4 :vec4)
                                                            ('raylib:texture :sampler2d)
                                                            ('raylib:matrix :vec4))))
                                 :collect (case slot-type
                                            (raylib:color
                                             `(clet ((,temp (foreign-alloca '(:struct raylib:vector4))))
                                                (setf (-> ,temp raylib:x) (/ (coerce (-> ,struct-instance ,slot-name raylib:r) 'single-float) 255.0)
                                                      (-> ,temp raylib:y) (/ (coerce (-> ,struct-instance ,slot-name raylib:g) 'single-float) 255.0)
                                                      (-> ,temp raylib:z) (/ (coerce (-> ,struct-instance ,slot-name raylib:b) 'single-float) 255.0)
                                                      (-> ,temp raylib:w) (/ (coerce (-> ,struct-instance ,slot-name raylib:a) 'single-float) 255.0))
                                                ,generic-form))
                                            (boolean
                                             `(clet ((,temp (foreign-alloca :int)))
                                                (setf ([] ,temp) (if (-> ,struct-instance ,slot-name) 1 0))
                                                ,generic-form))
                                            (raylib:texture
                                             `(raylib:%set-shader-value-texture ,shader (-> ,struct-instance ,(slot-name-location slot-name)) (& (-> ,struct-instance ,slot-name))))
                                            (raylib:matrix
                                             `(raylib:%set-shader-value-matrix ,shader (-> ,struct-instance ,(slot-name-location slot-name)) (& (-> ,struct-instance ,slot-name))))
                                            (t generic-form)))))))
                 (defmethod update-shader-uniforms ((,instance ,struct-name) ,shader)
                   (,updater-name ,instance ,shader)))))))))
