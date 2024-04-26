(in-package #:eon)

(defstruct (shadow-map-renderer (:constructor %make-shadow-map-renderer))
  (camera (raylib:make-camera-3d) :read-only t)
  (canvas nil :type scene2d-canvas :read-only t)
  (thunk #'values :type function)
  (matrix (raylib:make-matrix) :type raylib:matrix))

(defun make-shadow-map-renderer (&key
                                   (camera (raylib:make-camera-3d))
                                   (size (raylib:make-vector2
                                          :x (coerce +world-viewport-default-width+ 'single-float)
                                          :y (coerce +world-viewport-default-height+ 'single-float)))
                                   (filter :bilinear)
                                 &aux renderer)
  (setf renderer (%make-shadow-map-renderer
                  :camera camera
                  :canvas (scene2d-construct
                           (scene2d-canvas
                            :size size
                            :renderer (lambda ()
                                        (raylib:with-mode-3d (shadow-map-renderer-camera renderer)
                                          (clet ((proj (foreign-alloca '(:struct raylib:matrix)))
                                                 (view (foreign-alloca '(:struct raylib:matrix))))
                                            (rlgl:%get-matrix-projection proj)
                                            (rlgl:%get-matrix-modelview view)
                                            (raylib:%matrix-multiply (& (shadow-map-renderer-matrix renderer)) proj view))
                                          (funcall (shadow-map-renderer-thunk renderer))))))))
  (raylib:set-texture-filter
   (texture-region-texture (scene2d-canvas-content (shadow-map-renderer-canvas renderer)))
   (foreign-enum-value 'raylib:texture-filter filter))
  renderer)

(defun shadow-map-renderer-texture (renderer)
  (texture-region-texture (scene2d-canvas-content (shadow-map-renderer-canvas renderer))))

(defun shadow-map-renderer-render (renderer &optional thunk)
  (when thunk (setf (shadow-map-renderer-thunk renderer) thunk))
  (scene2d-canvas-render (shadow-map-renderer-canvas renderer))
  (let ((thunk (shadow-map-renderer-thunk renderer)))
    (unless (eq thunk #'values) thunk)))
