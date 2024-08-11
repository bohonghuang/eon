(in-package #:eon)

(defgeneric screen-render (screen)
  (:method ((function function))
    (funcall function))
  (:method ((null null))
    (raylib:clear-background raylib:+black+))
  (:documentation "Render SCREEN. Anything that specializes this generic function can be considered a screen, such as built-in types FUNCTION and NULL."))

(declaim (ftype (function (t)) screen-manager-render-default))

(defstruct (screen-manager (:include post-effect-manager))
  "A manager for handling screen rendering and transitions."
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
  "Set the render target during the execution of BODY to the internal RAYLIB:RENDER-TEXTURE of SCREEN-MANAGER."
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
  "Get the current screen."
  (screen-manager-screen screen-manager))

(defun (setf current-screen) (screen &optional (screen-manager *screen-manager*))
  "Set the current screen."
  (setf (screen-manager-screen screen-manager) screen))

(defun take-screenshot (&optional (screen-manager *screen-manager*))
  "Capture a snapshot of the current screen and return it as a RAYLIB:IMAGE."
  (load-asset 'raylib:image (raylib:render-texture-texture (screen-manager-render-texture screen-manager))))

(defun do-screen-loop (&optional (viewport (make-fit-viewport)) (background raylib:+black+))
  "Use a SCREEN-MANAGER to handle the game loop and ensure that the content of the screen is drawn within VIEWPORT. The content outside the viewport will be cleared to BACKGROUND."
  (let ((screen-manager *screen-manager*))
    (let ((texture (raylib:render-texture-texture
                    (screen-manager-render-texture screen-manager)))
          (width (viewport-width viewport))
          (height (viewport-height viewport)))
      (unless (and (= (raylib:texture-width texture) width)
                   (= (raylib:texture-height texture) height))
        (setf (screen-manager-render-texture screen-manager) (load-asset 'raylib:render-texture nil :width width :height height)
              (screen-manager-vertically-flipped-render-texture screen-manager) (load-asset 'raylib:render-texture nil :width width :height height))))
    (do-game-loop
      (screen-manager-update screen-manager)
      (raylib:with-drawing
        (typecase background
          (raylib:color (raylib:clear-background background))
          (function (funcall background))
          (t (scene2d-draw-simple background)))
        (with-viewport viewport
          (screen-manager-render screen-manager))))))

(deftype screen-transition () 'screen-manager-update-function)

(defgeneric ensure-screen-transition (object)
  (:method ((function function)) (values function (ute:timeline (:sequence))))
  (:method ((null null)) (ensure-screen-transition #'values)))

(defvar *shader-screen-transition-shader-uniforms*)

(defstruct shader-screen-transition
  (shader (car *shader-screen-transition-shader-uniforms*) :type raylib:shader :read-only t)
  (shader-uniforms (cdr *shader-screen-transition-shader-uniforms*) :type cobj:cobject :read-only t))

(defmethod ensure-screen-transition ((transition shader-screen-transition))
  (let ((shader (shader-screen-transition-shader transition)))
    (values
     (lambda (screen-manager)
       (with-screen-manager-mode screen-manager
         (raylib:with-shader-mode shader
           (update-shader-uniforms
            (shader-screen-transition-shader-uniforms transition)
            (shader-screen-transition-shader transition))
           (screen-manager-render screen-manager))))
     (ute:timeline (:sequence)))))

(defmacro define-shader-screen-transition ((name source) &body uniforms)
  (with-gensyms ( type arg args)
    `(progn
       (defstruct (,name (:include shader-screen-transition)
                         (:constructor ,(symbolicate '#:%make- name))))
       (define-shaderable-uniforms ,name . ,uniforms)
       (defun ,(symbolicate '#:make- name) (&rest ,args)
         (declare (dynamic-extent ,args))
         (let ((*shader-screen-transition-shader-uniforms*
                 (cons (load-asset 'raylib:shader ,source)
                       (,(symbolicate '#:make- name '#:-shader-uniforms)))))
           (apply #',(symbolicate '#:%make- name)
                  :shader-uniforms (or (getf ,args :shader-uniforms) (apply #',(symbolicate '#:make- name '#:-shader-uniforms) (delete-from-plist ,args :shader-uniforms :shader)))
                  (when-let ((,arg (getf ,args :shader))) (list :shader ,arg)))))
       (defmethod make-screen-transition ((,type null) (,name (eql ',name)) &rest ,args)
         (declare (dynamic-extent ,args) (ignore ,type ,name))
         (apply #',(symbolicate '#:make- name) ,args)))))

(defgeneric make-screen-transition (type name &rest args)
  (:documentation "Make a SCREEN-TRANSITION named NAME of transition TYPE with ARGS."))

(defmacro define-simple-shader-screen-transition ((name source) &body uniforms)
  "Define a SCREEN-TRANSITION named NAME from SOURCE with UNIFORMS. SOURCE is the SOURCE in (LOAD-ASSET 'RAYLIB:SHADER SOURCE) used for loading the shader. The loaded shader should contain a uniform named \"progress\" of type float. The syntax and requirements for UNIFORMS are consistent with the SLOTS of DEFINE-SHADERABLE-UNIFORMS."
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
  "Play TRANSITION and a PROMISE:PROMISE is fulfilled when this procedure is done."
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
  "Play TRANSITION-OUT, set the current screen to TARGET-SCREEN, and then play TRANSITION-IN. The returned PROMISE:PROMISE is fulfilled when this procedure is done."
  (async
    (await (promise-play-screen-transition transition-out))
    (setf (current-screen) target-screen)
    (await (promise-play-screen-transition transition-in))))

(setf (fdefinition 'transition-screen) (fdefinition 'promise-transition-screen))
