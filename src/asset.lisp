(in-package #:eon)

(defstruct asset-info
  (source nil :type list)
  (data nil :type t)
  (type nil :type symbol)
  (reference-counter 0 :type non-negative-fixnum))

(defstruct asset-type
  (name nil :type symbol :read-only t)
  (source-info (make-hash-table :test #'equal) :type hash-table)
  (data-info (make-hash-table :test #'equal) :type hash-table))

(defstruct asset-manager
  (type-table (make-hash-table) :type hash-table))

(defvar *asset-manager* nil)

(defgeneric load-asset (asset-type source &key &allow-other-keys)
  (:documentation "Load an asset of ASSET-TYPE from SOURCE."))

(defgeneric unload-asset (asset)
  (:documentation "Unload ASSET to release the associated resources. After calling, it must be ensured that the ASSET is no longer being used or repeatedly unloaded. Typically, this function does not need to be manually called, as reusable resources can ensure they are loaded once only, while non-reusable resources can be managed by the GC, even though manually unloading them can help reduce GC pause time."))

(defun asset-loaded-p (asset &aux (asset-type (type-of asset)))
  "Return non-NIL if ASSET has already been loaded."
  (with-accessors ((type-table asset-manager-type-table)) *asset-manager*
    (with-accessors ((data-info asset-type-data-info))
        (or (gethash asset-type type-table) (return-from asset-loaded-p nil))
      (gethash asset data-info))))

(defun unload-asset-finalizer (&rest data-list)
  (let ((context (tg:make-weak-pointer *game-loop-context*)))
    (lambda ()
      (when-let ((*game-loop-context* (tg:weak-pointer-value context)))
        (add-game-loop-hook (lambda ()
                              (loop :for data :in data-list
                                    :when (asset-loaded-p data)
                                      :do (unload-asset data)))
                            :before nil)))))

(defun register-unshareable-asset (data
                                   &optional
                                     (finalized nil)
                                     (finalizer #'unload-asset-finalizer)
                                   &aux
                                     (asset-type (type-of data)))
  (with-accessors ((type-table asset-manager-type-table)) *asset-manager*
    (with-accessors ((data-info asset-type-data-info))
        (ensure-gethash asset-type type-table (make-asset-type :name asset-type))
      (when (gethash data data-info)
        (error "Asset ~A has already been loaded." data))
      (setf (gethash data data-info) (make-asset-info :type asset-type :data data :reference-counter 1))))
  (if finalized
      (progn
        (assert (not (eq data finalized)))
        (tg:finalize finalized (funcall finalizer data)))
      data))

(defun deregister-unshareable-asset (data &optional (test #'eql) &aux (asset-type (type-of data)))
  (with-accessors ((type-table asset-manager-type-table)) *asset-manager*
    (with-accessors ((data-info asset-type-data-info))
        (ensure-gethash asset-type type-table (error "No ~A is loaded." asset-type))
      (if-let ((loaded-data (loop :for loaded-data :being :the hash-key :in data-info
                                  :when (funcall test loaded-data data)
                                    :return loaded-data)))
        (progn (remhash loaded-data data-info) loaded-data)
        (error "Asset ~A is not loaded." data)))))

(defmacro with-registered-unshareable-asset ((data-form &optional test) &body body)
  (with-gensyms (data)
    `(let ((,data ,data-form))
       (if (asset-loaded-p ,data)
           (progn . ,body)
           (progn
             (register-unshareable-asset ,data)
             (unwind-protect (progn . ,body)
               (deregister-unshareable-asset ,data . ,(when test (list test)))))))))

(defmethod load-asset :around (asset-type source &key)
  (with-accessors ((type-table asset-manager-type-table)) *asset-manager*
    (with-accessors ((source-info asset-type-source-info)
                     (data-info asset-type-data-info))
        (ensure-gethash asset-type type-table (make-asset-type :name asset-type))
      (let ((info (ensure-gethash source source-info
                                  (let* ((data (call-next-method))
                                         (info (ensure-gethash data data-info (make-asset-info :data data :source nil))))
                                    (assert (eq (type-of data) asset-type))
                                    (pushnew source (asset-info-source info) :test #'equal) info))))
        (incf (asset-info-reference-counter info))
        (asset-info-data info)))))

(defmethod load-asset (asset-type (symbol symbol) &rest args)
  (unless symbol (error "The symbol for loading asset cannot be NIL."))
  (apply #'load-asset asset-type (symbol-value symbol) args))

(define-condition asset-not-loaded-error (error)
  ((type :initform nil :initarg :type)
   (data :initform nil :initarg :data))
  (:report (lambda (condition stream)
             (with-slots (type data) condition
               (if data
                   (format stream "Asset ~A is not loaded." data)
                   (format stream "No asset of type ~A is loaded." type))))))

(defmethod unload-asset :around (data &aux (asset-type (type-of data)))
  (restart-case
      (with-accessors ((type-table asset-manager-type-table)) *asset-manager*
        (with-accessors ((source-info asset-type-source-info)
                         (data-info asset-type-data-info))
            (ensure-gethash asset-type type-table (error 'asset-not-loaded-error :type asset-type))
          (let ((info (ensure-gethash data data-info (error 'asset-not-loaded-error :type asset-type :data data))))
            (when (zerop (decf (asset-info-reference-counter info)))
              (call-next-method)
              (mapc (rcurry #'remhash source-info) (asset-info-source info))
              (remhash data data-info)))))
    (skip-unloading-asset ()
      :report (lambda (stream) (format stream "Skip unloading asset ~A." data)))))

(defstruct asset-group
  (table (make-hash-table :test #'equal) :type hash-table))

(defmethod load-asset ((asset-type (eql 'asset-group)) (asset-alist list) &key)
  (loop :with asset-group := (make-asset-group)
        :for (name . load-asset-args) :in asset-alist
        :do (setf (gethash name (asset-group-table asset-group)) (apply #'load-asset load-asset-args))
        :finally (return asset-group)))

(defmethod unload-asset ((group asset-group))
  (loop :for asset :being :the hash-value :in (asset-group-table group)
        :do (unload-asset asset)))

(defun loaded-assets (&optional (manager *asset-manager*))
  (loop :for asset-type :being :the hash-value :in (asset-manager-type-table manager)
        :nconc (loop :for asset-info :being :the hash-value :in (asset-type-data-info asset-type)
                     :collect (asset-info-data asset-info))))

(defmacro with-asset-manager (&body body)
  `(let* ((*asset-manager* (make-asset-manager)))
     (unwind-protect (progn . ,body)
       (handler-bind ((asset-not-loaded-error (lambda (c) (declare (ignore c)) (invoke-restart 'skip-unloading-asset))))
         (loop :for loaded-assets := (loaded-assets) :while loaded-assets :do (mapc #'unload-asset loaded-assets))))))

(defmethod load-asset ((asset-type (eql 'raylib:texture)) (path pathname) &key)
  (raylib:load-texture (namestring path)))

(defmethod load-asset ((asset-type (eql 'raylib:texture)) source &rest args)
  (raylib:load-texture-from-image (apply #'load-asset 'raylib:image source args)))

(defmethod load-asset :around ((asset-type (eql 'raylib:texture)) source &key)
  (let ((texture (call-next-method)))
    (assert (and (plusp (raylib:texture-width texture)) (plusp (raylib:texture-height texture))) (texture)
            "The dimension of ~A is invalid (~Dx~D), possibly due to the corrupted or non-existent loading source ~S."
            texture (raylib:texture-width texture) (raylib:texture-height texture) source)
    texture))

(defmethod unload-asset ((texture raylib:texture))
  (raylib:unload-texture texture))

(defmethod load-asset ((asset-type (eql 'raylib:image)) (path pathname) &key)
  (raylib:load-image (namestring path)))

(defmethod unload-asset ((image raylib:image))
  (raylib:unload-image image))

(defmethod load-asset ((asset-type (eql 'raylib:music)) (path pathname) &key)
  (raylib:load-music-stream (namestring path)))

(defmethod unload-asset ((music raylib:music))
  (raylib:unload-music-stream music))

(defmethod load-asset ((asset-type (eql 'raylib:sound)) (path pathname) &key)
  (raylib:load-sound (namestring path)))

(defmethod unload-asset ((sound raylib:sound))
  (raylib:unload-sound sound))

(defmethod load-asset ((asset-type (eql 'raylib:model)) (path pathname) &key)
  (let ((model (raylib:load-model (namestring path))))
    (throw 'load-asset (register-unshareable-asset model (cobj:pointer-cobject (& model) 'raylib:model)))))

(defmethod load-asset :around ((asset-type (eql 'raylib:model)) (path pathname) &key)
  (declare (ignore asset-type path))
  (catch 'load-asset (call-next-method)))

(defmethod unload-asset ((model raylib:model))
  (let ((model (deregister-unshareable-asset model #'cobj:cobject-eq)))
    (throw 'unload-asset (raylib:unload-model model))))

(defmethod unload-asset :around ((model raylib:model))
  (with-registered-unshareable-asset (model #'cobj:cobject-eq)
    (catch 'unload-asset (call-next-method))))

(defstruct (raylib::model-animations (:include cobj:carray)))

(export 'raylib::model-animations :raylib)

(defcstruct cint
  (value :int))

(cobj:define-cobject-class (:struct cint))

(defmethod load-asset ((asset-type (eql 'raylib::model-animations)) (path pathname) &key)
  (let* ((cint (make-cint))
         (cpointer (raylib:load-model-animations (namestring path) cint))
         (cobject (cobj:manage-cobject
                   (make-model-animations
                    :pointer (cobj:cobject-pointer cpointer)
                    :element-type (cobj::cpointer-element-type cpointer)
                    :dimensions (list (cint-value cint))))))
    (throw 'load-asset (register-unshareable-asset cobject (copy-model-animations cobject)))))

(defmethod load-asset :around ((asset-type (eql 'raylib::model-animations)) (path pathname) &key)
  (declare (ignore asset-type path))
  (catch 'load-asset (call-next-method)))

(defmethod unload-asset ((animations raylib::model-animations))
  (let ((animations (deregister-unshareable-asset animations #'cobj:cobject-eq)))
    (throw 'unload-asset (raylib:%unload-model-animations (cobj:unmanage-cobject animations) (cobj:clength animations)))))

(defmethod unload-asset :around ((animations raylib::model-animations))
  (with-registered-unshareable-asset (animations #'cobj:cobject-eq)
    (catch 'unload-asset (call-next-method))))

(defmethod load-asset ((asset-type (eql 'raylib:font)) (path pathname) &key)
  (raylib:load-font (namestring path)))

(defmethod unload-asset ((font raylib:font))
  (raylib:unload-font font))

(defmethod load-asset :around ((asset-type (eql 'raylib:texture)) (image raylib:image) &key)
  (catch 'load-asset (call-next-method)))

(defmethod load-asset ((asset-type (eql 'raylib:texture)) (image raylib:image) &key)
  (let* ((texture (raylib:load-texture-from-image image)))
    (throw 'load-asset (register-unshareable-asset texture (cobj:pointer-cobject (& texture) 'raylib:texture)))))

(defmethod unload-asset :around ((texture raylib:texture))
  (if (asset-loaded-p texture)
      (call-next-method)
      (with-registered-unshareable-asset (texture #'cobj:cobject-eq)
        (catch 'unload-asset (call-next-method)))))

(defmethod load-asset :around ((asset-type (eql 'raylib:image)) (data vector) &key format)
  (declare (ignore asset-type data format))
  (catch 'load-asset (call-next-method)))

(defmethod load-asset ((asset-type (eql 'raylib:image)) (data vector) &key format)
  (declare (type (simple-array (unsigned-byte 8) (*)) data))
  (let ((image (with-pointer-to-vector-data (pointer data)
                 (raylib:load-image-from-memory
                  (concatenate 'string "." (string-downcase (symbol-name format)))
                  (cobj:pointer-cpointer pointer '(unsigned-byte 8))
                  (length data)))))
    (throw 'load-asset (register-unshareable-asset image (cobj:pointer-cobject (& image) 'raylib:image)))))

(defmethod load-asset :around ((asset-type (eql 'raylib:image)) (source null) &key width height (format :uncompressed-r8g8b8a8))
  (declare (ignore asset-type source width height format))
  (catch 'load-asset (call-next-method)))

(defmethod load-asset ((asset-type (eql 'raylib:image)) (source null) &key width height (format :uncompressed-r8g8b8a8))
  (let* ((element-type (list 'unsigned-byte (* 8 (raylib:get-pixel-data-size 1 1 (foreign-enum-value 'raylib:pixel-format format)))))
         (image (raylib:make-image :width width :height height :format (foreign-enum-value 'raylib:pixel-format format)
                                   :mipmaps 1 :data (cobj:pointer-cpointer
                                                     (cobj:unmanage-cobject
                                                      (cobj:make-carray
                                                       (* width height)
                                                       :element-type element-type))
                                                     element-type))))
    (throw 'load-asset (register-unshareable-asset image (cobj:pointer-cobject (& image) 'raylib:image)))))

(defmethod load-asset :around ((asset-type (eql 'raylib:image)) (texture raylib:texture) &key)
  (declare (ignore asset-type texture))
  (catch 'load-asset (call-next-method)))

(defmethod load-asset ((asset-type (eql 'raylib:image)) (texture raylib:texture) &key)
  (let ((image (raylib:load-image-from-texture texture)))
    (throw 'load-asset (register-unshareable-asset image (cobj:pointer-cobject (& image) 'raylib:image)))))

(defmethod load-asset :around ((asset-type (eql 'raylib:image)) (image raylib:image) &key region)
  (declare (ignore asset-type image region))
  (catch 'load-asset (call-next-method)))

(defmethod load-asset ((asset-type (eql 'raylib:image)) (image raylib:image) &key region)
  (let ((image (if region (raylib:image-from-image image region) (raylib:image-copy image))))
    (throw 'load-asset (register-unshareable-asset image (cobj:pointer-cobject (& image) 'raylib:image)))))

(defmethod unload-asset :around ((image raylib:image))
  (if (asset-loaded-p image)
      (call-next-method)
      (with-registered-unshareable-asset (image #'cobj:cobject-eq)
        (catch 'unload-asset (call-next-method)))))

(defmethod load-asset ((asset-type (eql 'raylib:shader)) (path pathname) &key)
  (let ((shader (switch ((pathname-type path) :test #'equal)
                  ("fs" (raylib:load-shader (null-pointer) (namestring path)))
                  ("vs" (raylib:load-shader (namestring path) (null-pointer)))
                  (nil (raylib:load-shader
                        (concatenate 'string (namestring path) ".vs")
                        (concatenate 'string (namestring path) ".fs"))))))
    (throw 'load-asset (register-unshareable-asset shader (cobj:pointer-cobject (& shader) 'raylib:shader)))))

(defmethod load-asset :around ((asset-type (eql 'raylib:shader)) (path pathname) &key)
  (declare (ignore asset-type path))
  (catch 'load-asset (call-next-method)))

(defmethod load-asset ((asset-type (eql 'raylib:shader)) (code string) &key)
  (let ((position-before-version (search "#version" code))
        (position-after-version (1+ (position #\Newline code))))
    (assert (< position-before-version position-after-version))
    (let ((code-before-version (subseq code position-before-version position-after-version))
          (code-after-version (subseq code position-after-version)))
      (let ((shader (raylib:load-shader-from-memory
                     (if (search "defined(VERTEX)" code)
                         (format nil "~A#define VERTEX~%~A" code-before-version code-after-version)
                         (null-pointer))
                     (if (or (search "defined(FRAGMENT)" code))
                         (format nil "~A#define FRAGMENT~%~A" code-before-version code-after-version)
                         (null-pointer)))))
        (throw 'load-asset (register-unshareable-asset shader (cobj:pointer-cobject (& shader) 'raylib:shader)))))))

(defmethod load-asset :around ((asset-type (eql 'raylib:shader)) (code string) &key)
  (declare (ignore asset-type code))
  (catch 'load-asset (call-next-method)))

(defmethod unload-asset ((shader raylib:shader))
  (let ((shader (deregister-unshareable-asset shader #'cobj:cobject-eq)))
    (throw 'unload-asset (raylib:unload-shader shader))))

(defmethod unload-asset :around ((shader raylib:shader))
  (with-registered-unshareable-asset (shader #'cobj:cobject-eq)
    (catch 'unload-asset (call-next-method))))

(defmethod load-asset ((asset-type (eql 'raylib:render-texture)) (source null) &key (width 128) (height 128))
  (let ((texture (raylib:load-render-texture width height)))
    (throw 'load-asset (register-unshareable-asset texture (cobj:pointer-cobject (& texture) 'raylib:render-texture)))))

(defmethod load-asset :around ((asset-type (eql 'raylib:render-texture)) (source null) &key)
  (catch 'load-asset (call-next-method)))

(defmethod unload-asset ((texture raylib:render-texture))
  (let ((texture (deregister-unshareable-asset texture #'cobj:cobject-eq)))
    (throw 'unload-asset (raylib:unload-render-texture texture))))

(defmethod unload-asset :around ((texture raylib:render-texture))
  (with-registered-unshareable-asset (texture #'cobj:cobject-eq)
    (catch 'unload-asset (call-next-method))))

(defmethod load-asset ((asset-type (eql 'raylib:audio-stream)) (source null) &key (sample-rate 48000) (sample-size 32) (channels 2))
  (let ((audio-stream (raylib:load-audio-stream sample-rate sample-size channels)))
    (throw 'load-asset (register-unshareable-asset audio-stream (cobj:pointer-cobject (& audio-stream) 'raylib:audio-stream)))))

(defmethod load-asset :around ((asset-type (eql 'raylib:audio-stream)) (source null) &key)
  (catch 'load-asset (call-next-method)))

(defmethod unload-asset ((audio-stream raylib:audio-stream))
  (let ((audio-stream (deregister-unshareable-asset audio-stream #'cobj:cobject-eq)))
    (throw 'unload-asset (raylib:unload-audio-stream audio-stream))))

(defmethod unload-asset :around ((audio-stream raylib:audio-stream))
  (with-registered-unshareable-asset (audio-stream #'cobj:cobject-eq)
    (catch 'unload-asset (call-next-method))))
