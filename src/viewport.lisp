(in-package #:eon)

(defconstant +world-viewport-default-width+ 320)
(defconstant +world-viewport-default-height+ 192)

(defstruct viewport)

(defgeneric begin-viewport (viewport)
  (:documentation "Begin rendering the content of VIEWPORT."))

(defgeneric end-viewport (viewport)
  (:documentation "End rendering the content of VIEWPORT."))

(defmacro with-viewport (viewport &body body)
  "Evaluate BODY with the render target directed to VIEWPORT."
  (with-gensyms (viewport-var)
    `(let ((,viewport-var ,viewport))
       (begin-viewport ,viewport-var)
       (unwind-protect (progn . ,body)
         (end-viewport ,viewport-var)))))

(defstruct (screen-viewport (:include viewport))
  "A viewport making the world size equal to the screen size.")

(defmethod begin-viewport ((viewport screen-viewport))
  (declare (ignore viewport)))

(defmethod end-viewport ((viewport screen-viewport))
  (declare (ignore viewport)))

(defstruct (world-viewport (:include viewport) (:constructor nil))
  "A viewport with a world size different from the screen size."
  (width +world-viewport-default-width+ :type positive-fixnum)
  (height +world-viewport-default-height+ :type positive-fixnum))

(defstruct (stretch-viewport (:include world-viewport))
  "A WORLD-VIEWPORT that scales the world to take the whole screen.")

(defmethod begin-viewport ((viewport stretch-viewport))
  (let ((screen-width (coerce (raylib:get-screen-width) 'single-float))
        (screen-height (coerce (raylib:get-screen-height) 'single-float))
        (viewport-width (coerce (stretch-viewport-width viewport) 'single-float))
        (viewport-height (coerce (stretch-viewport-height viewport) 'single-float)))
    (rlgl:push-matrix)
    (rlgl:load-identity)
    (rlgl:scalef (/ screen-width viewport-width) (/ screen-height viewport-height) 1.0)))

(defmethod end-viewport ((viewport stretch-viewport))
  (declare (ignore viewport))
  (rlgl:pop-matrix))

(defstruct (fit-viewport (:include world-viewport))
  "A WORLD-VIEWPORT that scales the world up to fit the screen with aspect ratio kept.")

(progn
  (defmethod begin-viewport ((viewport fit-viewport))
    (let* #1=((screen-width (coerce (raylib:get-screen-width) 'single-float))
              (screen-height (coerce (raylib:get-screen-height) 'single-float))
              (viewport-width (coerce (fit-viewport-width viewport) 'single-float))
              (viewport-height (coerce (fit-viewport-height viewport) 'single-float))
              (scale (min (/ screen-width viewport-width) (/ screen-height viewport-height)))
              (scaled-width (* viewport-width scale))
              (scaled-height (* viewport-height scale))
              (offset-x (/ (- screen-width scaled-width) 2.0))
              (offset-y (/ (- screen-height scaled-height) 2.0)))
      (rlgl:push-matrix)
      (rlgl:load-identity)
      (rlgl:translatef offset-x offset-y 0.0)
      (rlgl:scalef scale scale 1.0)))
  (defmethod end-viewport ((viewport fit-viewport))
    (let* #1#
      (rlgl:load-identity)
      (raylib:draw-rectangle 0 0 (floor offset-x) (floor screen-height) raylib:+black+)
      (raylib:draw-rectangle (- (floor screen-width) (floor offset-x)) 0 (floor offset-x) (floor screen-height) raylib:+black+)
      (raylib:draw-rectangle (floor offset-x) 0 (floor scaled-width) (floor offset-y) raylib:+black+)
      (raylib:draw-rectangle (floor offset-x) (- (floor screen-height) (floor offset-y)) (floor scaled-width) (floor offset-y) raylib:+black+)
      (rlgl:pop-matrix))))
