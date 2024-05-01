(in-package #:eon)

(defconstant +world-viewport-default-width+ 320)
(defconstant +world-viewport-default-height+ 192)

(defmacro define-viewport-methods (viewport &rest methods)
  `(progn
     . ,(loop :for (method-type method-args . method-body) :in methods
              :for (function-name-components . method-name-components)
                := (cons (list viewport '#:- (symbol-name method-type))
                         (list '#:viewport '#:- (symbol-name method-type)))
              :for (function-name . method-name)
                := (progn
                     (when (member method-type '(:begin :end :draw))
                       (nreversef function-name-components)
                       (nreversef method-name-components))
                     (cons (apply #'symbolicate function-name-components)
                           (apply #'symbolicate method-name-components)))
              :nconc `((declaim (inline ,function-name))
                       (defun ,function-name ,method-args . ,method-body)
                       (defmethod ,method-name ,(cons (list (car method-args) viewport)
                                                 (cdr method-args))
                         (,function-name . ,method-args))))))

(defstruct viewport)

(defgeneric begin-viewport (viewport)
  (:documentation "Begin rendering the content of VIEWPORT."))

(defgeneric end-viewport (viewport)
  (:documentation "End rendering the content of VIEWPORT."))

(defgeneric draw-viewport (viewport)
  (:documentation "Draw the content of VIEWPORT onto current render target."))

(defgeneric viewport-width (viewport)
  (:documentation "Get the width of VIEWPORT's world."))

(defgeneric viewport-height (viewport)
  (:documentation "Get the height of VIEWPORT's world."))

(defmacro with-viewport (viewport &body body)
  "Evaluate BODY with the render target directed to VIEWPORT."
  (with-gensyms (viewport-var)
    `(let ((,viewport-var ,viewport))
       (begin-viewport ,viewport-var)
       (unwind-protect (progn . ,body)
         (end-viewport ,viewport-var))
       (draw-viewport ,viewport-var))))

(defstruct (screen-viewport (:include viewport))
  "A viewport making the world size equal to the screen size.")

(define-viewport-methods screen-viewport
  (:begin (viewport) (declare (ignore viewport)))
  (:end (viewport) (declare (ignore viewport)))
  (:draw (viewport) (declare (ignore viewport)))
  (:width (viewport) (declare (ignore viewport)) (raylib:get-screen-width))
  (:height (viewport) (declare (ignore viewport)) (raylib:get-screen-height)))

(defstruct (world-viewport (:include viewport)
                           (:constructor nil))
  "A viewport with a world size different from the screen size."
  render-texture)

(defun world-viewport-initialize (viewport width height)
  (let ((render-texture (load-asset 'raylib:render-texture nil :width width :height height)))
    (setf (world-viewport-render-texture viewport) render-texture)
    viewport))

(define-viewport-methods world-viewport
  (:begin (viewport)
    (raylib:begin-texture-mode (world-viewport-render-texture viewport)))
  (:end (viewport)
    (declare (ignore viewport))
    (raylib:end-texture-mode))
  (:draw (viewport) (declare (ignore viewport)))
  (:width (viewport)
    (let ((render-texture (& (world-viewport-render-texture viewport))))
      (clocally (declare (ctype (:pointer (:struct raylib:render-texture)) render-texture))
        (-> render-texture raylib:texture raylib:width))))
  (:height (viewport)
    (let ((render-texture (& (world-viewport-render-texture viewport))))
      (clocally (declare (ctype (:pointer (:struct raylib:render-texture)) render-texture))
        (-> render-texture raylib:texture raylib:height)))))

(defstruct (stretch-viewport (:include world-viewport)
                             (:constructor %make-stretch-viewport))
  "A WORLD-VIEWPORT that scales the world to take the whole screen.")

(defun make-stretch-viewport (&key (width +world-viewport-default-width+) (height +world-viewport-default-height+))
  (world-viewport-initialize (%make-stretch-viewport) width height))

(define-viewport-methods stretch-viewport
  (:begin (viewport) (begin-world-viewport viewport))
  (:end (viewport) (end-world-viewport viewport))
  (:draw (viewport)
    (clet* ((render-texture (cthe (:pointer (:struct raylib:render-texture)) (& (world-viewport-render-texture viewport))))
            (texture (& (-> render-texture raylib:texture)))
            (source (foreign-alloca '(:struct raylib:rectangle)))
            (dest (foreign-alloca '(:struct raylib:rectangle)))
            (origin (foreign-alloca '(:struct raylib:vector2))))
      (let ((world-width (coerce (-> texture raylib:width) 'single-float))
            (world-height (coerce (-> texture raylib:height) 'single-float))
            (screen-width (coerce (raylib:get-screen-width) 'single-float))
            (screen-height (coerce (raylib:get-screen-height) 'single-float)))
        (setf (-> source raylib:x) 0.0
              (-> source raylib:y) world-height
              (-> source raylib:width) world-width
              (-> source raylib:height) (- world-height))
        (setf (-> dest raylib:x) 0.0
              (-> dest raylib:y) 0.0
              (-> dest raylib:width) screen-width
              (-> dest raylib:height) screen-height)
        (setf (-> origin raylib:x) 0.0
              (-> origin raylib:y) 0.0)
        (raylib:%draw-texture-pro texture source dest origin 0.0 (& raylib:+white+)))))
  (:width (viewport) (world-viewport-width viewport))
  (:height (viewport) (world-viewport-height viewport)))

(defstruct (fit-viewport (:include world-viewport)
                         (:constructor %make-fit-viewport))
  "A WORLD-VIEWPORT that scales the world up to fit the screen with aspect ratio kept.")

(defun make-fit-viewport (&key (width +world-viewport-default-width+) (height +world-viewport-default-height+))
  (world-viewport-initialize (%make-fit-viewport) width height))

(define-viewport-methods fit-viewport
  (:begin (viewport) (begin-world-viewport viewport))
  (:end (viewport) (end-world-viewport viewport))
  (:draw (viewport)
    (clet* ((render-texture (cthe (:pointer (:struct raylib:render-texture)) (& (world-viewport-render-texture viewport))))
            (texture (& (-> render-texture raylib:texture)))
            (source (foreign-alloca '(:struct raylib:rectangle)))
            (dest (foreign-alloca '(:struct raylib:rectangle)))
            (origin (foreign-alloca '(:struct raylib:vector2))))
      (let ((world-width (coerce (-> texture raylib:width) 'single-float))
            (world-height (coerce (-> texture raylib:height) 'single-float))
            (screen-width (coerce (raylib:get-screen-width) 'single-float))
            (screen-height (coerce (raylib:get-screen-height) 'single-float)))
        (let ((world-aspect (/ world-width world-height))
              (screen-aspect (/ screen-width screen-height)))
          (setf (-> source raylib:x) 0.0
                (-> source raylib:y) world-height
                (-> source raylib:width) world-width
                (-> source raylib:height) (- world-height))
          (cond
            ((< world-aspect screen-aspect)
             (let ((render-width (* screen-height world-aspect))
                   (render-height screen-height))
               (setf (-> dest raylib:x) (/ (- screen-width render-width) 2)
                     (-> dest raylib:y) 0.0
                     (-> dest raylib:width) render-width
                     (-> dest raylib:height) render-height)))
            ((> world-aspect screen-aspect)
             (let ((render-width screen-width)
                   (render-height (/ screen-width world-aspect)))
               (setf (-> dest raylib:x) 0.0
                     (-> dest raylib:y) (/ (- screen-height render-height) 2)
                     (-> dest raylib:width) render-width
                     (-> dest raylib:height) render-height)))
            (t (setf (-> dest raylib:x) 0.0
                     (-> dest raylib:y) 0.0
                     (-> dest raylib:width) screen-width
                     (-> dest raylib:height) screen-height)))
          (setf (-> origin raylib:x) 0.0
                (-> origin raylib:y) 0.0)
          (raylib:%draw-texture-pro texture source dest origin 0.0 (& raylib:+white+))))))
  (:width (viewport) (world-viewport-width viewport))
  (:height (viewport) (world-viewport-height viewport)))
