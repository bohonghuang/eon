(in-package #:eon)

(defstruct (post-effect-manager (:constructor %make-post-effect-manager))
  (render-texture
   (load-asset 'raylib:render-texture nil
               :width (raylib:get-screen-width)
               :height (raylib:get-screen-height))
   :type raylib:render-texture)
  (vertically-flipped-render-texture
   (load-asset 'raylib:render-texture nil
               :width (raylib:get-screen-width)
               :height (raylib:get-screen-height))
   :type raylib:render-texture)
  (alpha-unpremultiply-shader
   (load-asset 'raylib:shader
"#version 330
#if defined(FRAGMENT)
in vec2 fragTexCoord;
in vec4 fragColor;

uniform sampler2D texture0;

out vec4 finalColor;

void main() {
  finalColor = texture(texture0, fragTexCoord) * fragColor;
  if (finalColor.a > 0) {
    finalColor.rgb = finalColor.rgb / finalColor.a;
  }
}
#endif")
   :type raylib:shader))

(defun make-post-effect-manager (&key (size (raylib:make-vector2
                                             :x #.(float +world-viewport-default-width+)
                                             :y #.(float +world-viewport-default-height+))))
  (%make-post-effect-manager
   :render-texture (load-asset 'raylib:render-texture nil
                               :width (floor (raylib:vector2-x size))
                               :height (floor (raylib:vector2-y size)))
   :vertically-flipped-render-texture (load-asset 'raylib:render-texture nil
                                                  :width (floor (raylib:vector2-x size))
                                                  :height (floor (raylib:vector2-y size)))))

(defun post-effect-manager-size (post-effect-manager)
  (let ((texture (raylib:render-texture-texture (post-effect-manager-render-texture post-effect-manager))))
    (raylib:make-vector2
     :x (coerce (raylib:texture-width texture) 'single-float)
     :y (coerce (raylib:texture-height texture) 'single-float))))

(defun post-effect-manager-draw (post-effect-manager)
  (clet* ((render-texture (cthe (:pointer (:struct raylib:render-texture)) (& (post-effect-manager-render-texture post-effect-manager))))
          (texture (& (-> render-texture raylib:texture))))
    (raylib:%draw-texture texture 0 0 (& raylib:+white+))))

(defun post-effect-manager-begin-texture-mode (render-texture)
  (raylib:begin-texture-mode render-texture)
  (rlgl:set-blend-factors-separate #.rlgl:+src-alpha+ #.rlgl:+one-minus-src-alpha+ #.rlgl:+one+ #.rlgl:+one+ #.rlgl:+func-add+ #.rlgl:+max+)
  (raylib:begin-blend-mode #.(foreign-enum-value 'rlgl:blend-mode :custom-separate)))

(defun post-effect-manager-end-texture-mode ()
  (raylib:end-blend-mode)
  (raylib:end-texture-mode))

(defun post-effect-manager-begin (post-effect-manager)
  (post-effect-manager-begin-texture-mode (post-effect-manager-vertically-flipped-render-texture post-effect-manager)))

(defun post-effect-manager-end (post-effect-manager)
  (post-effect-manager-end-texture-mode)
  (raylib:with-texture-mode (post-effect-manager-render-texture post-effect-manager)
    (raylib:clear-background raylib:+blank+)
    (raylib:with-shader-mode (post-effect-manager-alpha-unpremultiply-shader post-effect-manager)
      (clet* ((render-texture (cthe (:pointer (:struct raylib:render-texture)) (& (post-effect-manager-vertically-flipped-render-texture post-effect-manager))))
              (texture (& (-> render-texture raylib:texture))))
        (rlgl:disable-color-blend)
        (raylib:%draw-texture texture 0 0 (& raylib:+white+))
        (rlgl:draw-render-batch-active)
        (rlgl:enable-color-blend)))))

(defmacro with-post-effect-manager-mode (post-effect-manager &body body)
  (once-only (post-effect-manager)
    `(progn
       (post-effect-manager-begin ,post-effect-manager)
       (unwind-protect (progn . ,body)
         (post-effect-manager-end ,post-effect-manager)))))

(defstruct (post-effect-viewport (:constructor %make-post-effect-viewport))
  "A VIEWPORT that allows the rendered content to be processed by its PROCESSOR and then rendered onto its inner VIEWPORT."
  (viewport (make-screen-viewport) :type viewport)
  (manager (make-post-effect-manager) :type post-effect-manager)
  (processor #'funcall :type function))

(defun make-post-effect-viewport (&key
                                    (viewport (make-screen-viewport))
                                    (width (raylib:get-screen-width))
                                    (height (raylib:get-screen-height))
                                    (size (raylib:make-vector2 :x (coerce width 'single-float) :y (coerce height 'single-float)))
                                    (manager (make-post-effect-manager :size size))
                                    (processor #'funcall))
  "Construct a POST-EFFECT-VIEWPORT with dimensions WIDTH and HEIGHT, and receive a VIEWPORT as its inner (parent) VIEWPORT. The PROCESSOR will be invoked with a function as an argument when the POST-EFFECT-VIEWPORT is drawn, and calling the function will render the content originally rendered to that VIEWPORT. Any content rendered in the PROCESSOR will be rendered onto its inner VIEWPORT."
  (%make-post-effect-viewport :viewport viewport :manager manager :processor processor))

(defmethod begin-viewport ((viewport post-effect-viewport))
  (post-effect-manager-begin (post-effect-viewport-manager viewport)))

(defmethod end-viewport ((viewport post-effect-viewport))
  (let ((post-effect-manager (post-effect-viewport-manager viewport)))
    (post-effect-manager-end post-effect-manager)
    (let ((draw-function (lambda () (post-effect-manager-draw post-effect-manager))))
      (declare (dynamic-extent draw-function))
      (with-viewport (post-effect-viewport-viewport viewport)
        (funcall (post-effect-viewport-processor viewport) draw-function)))))
