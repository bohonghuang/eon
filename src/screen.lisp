(in-package #:eon)

(defstruct post-effect-manager
  (render-texture
   (load-asset 'raylib:render-texture nil
               :width +world-viewport-default-width+
               :height +world-viewport-default-height+)
   :type raylib:render-texture :read-only t)
  (vertically-flipped-render-texture
   (load-asset 'raylib:render-texture nil
               :width +world-viewport-default-width+
               :height +world-viewport-default-height+)
   :type raylib:render-texture :read-only t))

(defun post-effect-manager-draw (post-effect-manager)
  (clet* ((render-texture (cthe (:pointer (:struct raylib:render-texture)) (& (post-effect-manager-render-texture post-effect-manager))))
          (texture (& (-> render-texture raylib:texture))))
    (raylib:%draw-texture texture 0 0 (& raylib:+white+))))

(defmacro with-post-effect-manager-mode (post-effect-manager &body body)
  (with-gensyms (render-texture texture)
    `(progn
       (raylib:with-texture-mode (post-effect-manager-vertically-flipped-render-texture ,post-effect-manager) . ,body)
       (raylib:with-texture-mode (post-effect-manager-render-texture ,post-effect-manager)
         (clet* ((,render-texture (cthe (:pointer (:struct raylib:render-texture)) (& (post-effect-manager-vertically-flipped-render-texture ,post-effect-manager))))
                 (,texture (& (-> ,render-texture raylib:texture))))
           (raylib:%draw-texture ,texture 0 0 (& raylib:+white+)))))))

(defgeneric screen-render (screen)
  (:method ((function function))
    (funcall function))
  (:method ((null null))
    (raylib:clear-background raylib:+black+)))

(declaim (ftype (function (t)) screen-manager-render-default))

(defstruct (screen-manager (:include post-effect-manager))
  (screen nil)
  (update-function #'screen-manager-update-default :type function)
  (render-function #'screen-manager-render-default :type function))

(deftype screen-manager-update-function ()
  `(function (screen-manager)))

(deftype screen-manager-render-function ()
  `(function (screen-manager)))

(declaim (special *screen-manager*))

(setf (assoc-value *game-special-bindings* '*screen-manager*) '(make-screen-manager))

(defmacro with-screen-manager-mode (screen-manager &body body)
  `(with-post-effect-manager-mode ,screen-manager . ,body))

(defun screen-manager-update-default (screen-manager)
  (with-screen-manager-mode screen-manager
    (screen-render (screen-manager-screen screen-manager))))

(defun screen-manager-render-default (screen-manager)
  (post-effect-manager-draw screen-manager))

(defun screen-manager-update (screen-manager)
  (funcall (screen-manager-update-function screen-manager) screen-manager))

(defun screen-manager-render (screen-manager)
  (funcall (screen-manager-render-function screen-manager) screen-manager))

(defun current-screen (&optional (screen-manager *screen-manager*))
  (screen-manager-screen screen-manager))

(defun (setf current-screen) (screen &optional (screen-manager *screen-manager*))
  (setf (screen-manager-screen screen-manager) screen))

(defun do-screen-loop (&optional (viewport (make-fit-viewport)))
  (do-game-loop
    (let ((screen-manager *screen-manager*))
      (screen-manager-update screen-manager)
      (raylib:with-drawing
        (with-viewport viewport
          (screen-manager-render screen-manager))))))

(deftype screen-transition () 'screen-manager-update-function)

(defgeneric ensure-screen-transition (object)
  (:method ((function function)) function))

(defstruct shader-screen-transition)

(defgeneric shader-screen-transition-shader (transition))

(defmethod ensure-screen-transition ((transition shader-screen-transition))
  (let ((shader (shader-screen-transition-shader transition)))
    (initialize-shaderable-uniforms transition)
    (values
     (lambda (screen-manager)
       (with-screen-manager-mode screen-manager
         (raylib:with-shader-mode shader
           (update-shaderable-uniforms transition)
           (screen-manager-render screen-manager))))
     (ute:timeline (:sequence)))))

(defmacro define-shader-screen-transition ((name source) &body uniforms)
  (with-gensyms (transition type arg args)
    `(progn
       (defstruct (,name (:include shader-screen-transition)
                         (:constructor ,(symbolicate '#:%make- name)))
         (shader (load-asset 'raylib:shader ,source) :type raylib:shader :read-only t)
         (shader-uniforms (,(symbolicate '#:make- name '#:-shader-uniforms)) :read-only t))
       (define-shaderable-uniforms ,name . ,uniforms)
       (defmethod shader-screen-transition-shader ((,transition ,name))
         (,(symbolicate name '#:-shader) ,transition))
       (defun ,(symbolicate '#:make- name) (&rest ,args)
         (declare (dynamic-extent ,args))
         (apply #',(symbolicate '#:%make- name)
                :shader-uniforms (or (getf ,args :shader-uniforms) (apply #',(symbolicate '#:make- name '#:-shader-uniforms) (delete-from-plist ,args :shader-uniforms :shader)))
                (when-let ((,arg (getf ,args :shader))) (list :shader ,arg))))
       (defmethod make-screen-transition ((,type null) (,name (eql ',name)) &rest ,args)
         (declare (dynamic-extent ,args) (ignore ,type ,name))
         (apply #',(symbolicate '#:make- name) ,args)))))

(defgeneric make-screen-transition (type name &rest args))

(defmacro define-simple-shader-screen-transition ((name source) &body uniforms)
  (let ((transition-in (symbolicate name '#:-in))
        (transition-out (symbolicate name '#:-out))
        (progress-accessor (symbolicate name '#:-progress)))
    (flet ((transition-definition (symbol source-progress target-progress)
             (with-gensyms (transition duration arg args)
               `(progn
                  (defstruct (,symbol (:constructor ,(symbolicate '#:%make- symbol)))
                    (transition (,(symbolicate '#:make- name)) :type ,name)
                    (duration 1.0 :type single-float))
                  (defun ,(symbolicate '#:make- symbol) (&rest ,args)
                    (declare (dynamic-extent ,args))
                    (apply #',(symbolicate '#:%make- symbol)
                           (nconc
                            (when-let ((,arg (getf ,args :duration)))
                              (list :duration ,arg))
                            (list :transition (if-let ((,arg (getf ,args :transition)))
                                                ,arg (apply #',(symbolicate '#:make- name) (delete-from-plist ,args :transition :duration)))))))
                  (defmethod ensure-screen-transition ((,transition ,symbol))
                    (let ((,transition (,(symbolicate symbol '#:-transition) ,transition))
                          (,duration (,(symbolicate symbol '#:-duration) ,transition)))
                      (setf (,progress-accessor ,transition) ,source-progress)
                      (values (ensure-screen-transition ,transition)
                              (ute:tween :to (((,progress-accessor ,transition)) (,target-progress))
                                         :ease #'ute:linear-inout :duration ,duration))))))))
      (with-gensyms (type args)
        `(progn
           (define-shader-screen-transition (,name ,source)
             ("progress" 0.0 :type single-float) . ,uniforms)
           ,(transition-definition transition-out 0.0 1.0)
           (defmethod make-screen-transition ((,type (eql :out)) (,name (eql ',name)) &rest ,args)
             (declare (dynamic-extent ,args) (ignore ,type ,name))
             (apply #',(symbolicate '#:make- transition-out) ,args))
           ,(transition-definition transition-in 1.0 0.0)
           (defmethod make-screen-transition ((,type (eql :in)) (,name (eql ',name)) &rest ,args)
             (declare (dynamic-extent ,args) (ignore ,type ,name))
             (apply #',(symbolicate '#:make- transition-in) ,args)))))))

(define-simple-shader-screen-transition (screen-transition-fade
"#version 330
#if defined(FRAGMENT)
in vec2 fragTexCoord;
in vec4 fragColor;

uniform sampler2D texture0;

out vec4 finalColor;

uniform float progress = 0.0;
uniform vec4 background = vec4(0.0);

void main() {
  finalColor = texture(texture0, fragTexCoord) * fragColor;
  finalColor = mix(finalColor, background, progress);
}
#endif")
  ("background" raylib:+black+ :type raylib:color))

(defun promise-play-screen-transition (transition)
  (multiple-value-bind (transition-update-function transition-tween) (ensure-screen-transition transition)
    (with-accessors ((update-function screen-manager-update-function))
        *screen-manager*
      (assert (eq update-function #'screen-manager-update-default))
      (let ((super-update-function update-function))
        (async
          (setf update-function (lambda (screen-manager)
                                  (funcall super-update-function screen-manager)
                                  (funcall transition-update-function screen-manager)))
          (await (promise-tween transition-tween))
          (setf update-function #'screen-manager-update-default))))))

(setf (fdefinition 'play-screen-transition) (fdefinition 'promise-play-screen-transition))

(defun promise-transition-screen (target-screen
                                  &optional
                                    (transition-out (make-screen-transition-fade-out :duration 0.25))
                                    (transition-in (make-screen-transition-fade-in :duration 0.25)))
  (async
    (await (promise-play-screen-transition transition-out))
    (setf (current-screen) target-screen)
    (await (promise-play-screen-transition transition-in))))

(setf (fdefinition 'transition-screen) (fdefinition 'promise-transition-screen))
