(in-package #:eon)

(defstruct (n-patch (:constructor %make-n-patch))
  (texture (raylib:make-texture :id 0 :width 1 :height 1 :mipmaps 0 :format 0) :type raylib:texture)
  (info (raylib:make-n-patch-info :source (raylib:make-rectangle :x 0.0 :y 0.0 :width 0.0 :height 0.0)
                                  :left 0 :top 0 :right 0 :bottom 0 :layout 0)
   :type raylib:n-patch-info))

(defun make-n-patch (&key texture body (layout :nine-patch))
  (let* ((texture-region (if (typep texture 'texture-region) texture (make-texture-region :texture texture)))
         (info (raylib:make-n-patch-info :source (texture-region-region texture-region)
                                         :left (truncate (raylib:rectangle-x body))
                                         :top (truncate (raylib:rectangle-y body))
                                         :right (truncate (if (minusp (raylib:rectangle-width body))
                                                              (- (raylib:rectangle-width body))
                                                              (+ (raylib:rectangle-x body) (raylib:rectangle-width body))))
                                         :bottom (truncate (if (minusp (raylib:rectangle-height body))
                                                               (- (raylib:rectangle-height body))
                                                               (- (raylib:rectangle-y body) (raylib:rectangle-height body))))
                                         :layout (foreign-enum-value 'raylib:n-patch-layout layout)))
         (texture (texture-region-texture texture-region)))
    (%make-n-patch :texture texture :info info)))

(defstruct text-style
  (font (raylib:get-font-default))
  (size 10.0 :type single-float)
  (spacing 1.0 :type single-float))

(defstruct text
  (string "" :type string)
  (style (make-text-style) :type text-style))
