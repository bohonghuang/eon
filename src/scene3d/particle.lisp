(in-package #:eon)

(defstruct (scene3d-particle-emitter (:include scene3d-node)
                                     (:constructor %make-scene3d-particle-emitter))
  "A SCENE3D-NODE that wraps a PARTICLE-3D-EMITTER."
  (emitter nil :type particle-3d-emitter)
  (updater nil :type function))

(defun sine-yoyo-2 (x)
  (declare (type single-float x))
  (cond
    ((< x 0.5) (ute:sine-out (* x 2.0)))
    ((< x 1.0) (ute:sine-in (* (- 1.0 x) 2.0)))
    (t 0.0)))

(defun sine-yoyo-3 (x)
  (declare (type single-float x))
  (cond
    ((< x 0.33) (ute:sine-out (* x 3.0)))
    ((< x 0.66) 1.0)
    ((< x 1.0) (ute:sine-in (* (- 1.0 x) 3.0)))
    (t 0.0)))

(defun particle-3d-scene3d-node-renderer (node
                                          &key
                                            (position +vector3-zeros+)
                                            (origin +vector3-zeros+)
                                            (scale
                                             (particle-3d-interpolate-vector3-over-age
                                              +vector3-zeros+ +vector3-ones+ #'sine-yoyo-3))
                                            (rotation +quaternion-identity+)
                                            (color
                                             (particle-3d-interpolate-color-over-age
                                              (raylib:fade raylib:+white+ 0.0)
                                              (raylib:fade raylib:+white+ 0.9)
                                              #'sine-yoyo-2)))
  "A PARTICLE-3D-RENDERER that renders particles using NODE. POSITION, ORIGIN, SCALE, ROTATION, and COLOR can be the parameters or their PARTICLE-3D-VALUE-GENERATORs used for SCENE3D-DRAW. This is suitable for use when drawing billboards with depth testing disabled."
  (let ((position-offset position)
        (rotation-offset rotation))
    (with-value-generators (node position-offset origin scale color rotation-offset)
      (declare (type (function (particle-3d) (values raylib:color)) color)
               (type (function (particle-3d) (values raylib:vector3)) position-offset scale)
               (type (function (particle-3d) (values raylib:vector4)) rotation-offset))
      (let ((position (raylib:make-vector3))
            (rotation (raylib:make-quaternion)))
        (lambda (particle)
          (let ((position-offset (funcall position-offset particle))
                (rotation-offset (funcall rotation-offset particle))
                (origin (funcall origin particle))
                (scale (funcall scale particle))
                (color (funcall color particle)))
            (clet ((particle (cthe (:pointer (:struct particle-3d)) (& particle))))
              (raylib:%vector3-add (& position) (& (-> particle position)) (& position-offset))
              (raylib:%quaternion-multiply (& rotation) (& (-> particle rotation)) (& rotation-offset)))
            (scene3d-draw (funcall node particle) position origin scale rotation color)))))))

(declaim (ftype (function (t) (values (function (&rest t) (values particle-3d-renderer)))) ensure-particle-3d-scene3d-renderer-constructor))
(defun ensure-particle-3d-scene3d-renderer-constructor (node)
  (etypecase node
    (scene3d-node (values (curry #'particle-3d-scene3d-node-renderer node)))
    (function node)))

(defun particle-3d-scene3d-billboard-renderer (node
                                               &rest
                                                 args
                                               &key
                                                 (epsilon 1e-2)
                                                 (position +vector3-zeros+)
                                                 (up (raylib:make-vector3 :x 0.0 :y 1.0 :z 0.0))
                                               &allow-other-keys)
  "Like PARTICLE-3D-SCENE3D-NODE-RENDERER, but use different depths offset by EPSILON for rendering different particles. This is applicable for rendering billboards with depth testing and alpha testing enabled."
  (declare (type single-float epsilon))
  (remove-from-plistf args :epsilon :position :up)
  (let ((offset position))
    (with-value-generators (offset)
      (declare (type (function (particle-3d) (values raylib:vector3)) offset))
      (let* ((total-offset (raylib:make-vector3))
             (renderer (apply (ensure-particle-3d-scene3d-renderer-constructor node) :position total-offset args))
             (orders (make-hash-table))
             (camera *scene3d-camera*))
        (lambda (particle)
          (let ((order (1- (ensure-gethash particle orders (1+ (hash-table-count orders)))))
                (offset (funcall offset particle)))
            (declare (type non-negative-fixnum order))
            (clet ((camera (cthe (:pointer (:struct raylib:camera-3d)) (& camera)))
                   (total-offset (cthe (:pointer (:struct raylib:vector3)) (& total-offset)))
                   (offset (cthe (:pointer (:struct raylib:vector3)) (& offset)))
                   (up (cthe (:pointer (:struct raylib:vector3)) (& up)))
                   (right (foreign-alloca '(:struct raylib:vector3))))
              (%billboard-right-vector right camera)
              (raylib:%vector3-cross-product total-offset right up)
              (raylib:%vector3-normalize total-offset total-offset)
              (raylib:%vector3-scale total-offset total-offset (* (coerce order 'single-float) epsilon))
              (raylib:%vector3-add total-offset total-offset offset)))
          (funcall renderer particle))))))

(declaim (ftype (function (t &rest t) (values particle-3d-renderer particle-3d-updater)) particle-3d-scene3d-node-sorting-renderer))
(defun particle-3d-scene3d-node-sorting-renderer (node &rest args)
  "Like PARTICLE-3D-SCENE3D-NODE-RENDERER, but sort the particles to be rendered from far to near to ensure proper rendering of transparency. This is applicable for rendering NODEs with transparency. The second return value of type PARTICLE-3D-UPDATER should used, in order to update the internal state of the renderer."
  (let* ((renderer (apply (ensure-particle-3d-scene3d-renderer-constructor node) args))
         (particles nil)
         (camera *scene3d-camera*))
    (declare (type list particles))
    (values
     (lambda (particle)
       (when (eq particle (first particles))
         (setf particles (sort particles #'> :key (lambda (particle)
                                                    (clet ((camera (cthe (:pointer (:struct raylib:camera-3d)) (& camera)))
                                                           (particle (cthe (:pointer (:struct particle-3d)) (& particle))))
                                                      (raylib:%vector3-distance
                                                       (& (-> camera raylib:position))
                                                       (& (-> particle position)))))))
         (loop :for particle := (pop particles)
               :while particle
               :do (funcall renderer particle))))
     (lambda (particle)
       (push particle particles)))))

(defun particle-3d-scene3d-bullet-renderer (node &rest args)
  "Like PARTICLE-3D-SCENE3D-NODE-RENDERER, but change its rotation based on the movement direction of the NODE on the plane facing *SCENE3D-CAMERA*."
  (let* ((rotation (raylib:copy-quaternion +quaternion-identity+))
         (renderer (apply (ensure-particle-3d-scene3d-renderer-constructor node) :rotation rotation args))
         (camera *scene3d-camera*))
    (declare (type (function (particle-3d)) renderer))
    (lambda (particle)
      (clet* ((particle (cthe (:pointer (:struct particle-3d)) (& particle)))
              (rotation (cthe (:pointer (:struct raylib:vector4)) (& rotation)))
              (velocity (& (-> particle position-velocity)))
              (camera (cthe (:pointer (:struct raylib:camera-3d)) (& camera))))
        (raylib:%quaternion-from-euler
         rotation 0.0 0.0
         (clet ((offset (foreign-alloca '(:struct raylib:vector3)))
                (cross (foreign-alloca '(:struct raylib:vector3))))
           (raylib:%vector3-subtract offset (& (-> camera raylib:position)) (& (-> particle position)))
           (raylib:%vector3-cross-product cross (& (-> camera raylib:up)) velocity)
           (float-sign (raylib:%vector3-dot-product offset cross)
                       (raylib:%vector3-angle velocity (& (-> camera raylib:up)))))))
      (funcall renderer particle))))

(declaim (ftype (function (&rest t) (values (function (&rest t) (values particle-3d-updater)))) scene3d-particle-emitter-billboard-updater))
(defun scene3d-particle-emitter-billboard-updater (&rest args)
  "A wrapper for PARTICLE-3D-BILLBOARD-UPDATER to be passed to MAKE-SCENE3D-PARTICLE-EMITTER."
  (values (apply #'rcurry (curry #'particle-3d-billboard-updater *scene3d-camera*) args)))

(declaim (ftype (function ((particle-3d-value-or-generator raylib:vector3) &rest t)
                          (values (function (&rest t) (values particle-3d-updater))))
                scene3d-particle-emitter-laser-updater))
(defun scene3d-particle-emitter-laser-updater (target &rest args)
  "A wrapper for PARTICLE-3D-LASER-UPDATER to be passed to MAKE-SCENE3D-PARTICLE-EMITTER."
  (values (apply #'rcurry #'particle-3d-laser-updater target args)))

(declaim (ftype (function ((particle-3d-value-or-generator raylib:vector3) &rest t)
                          (values (function (&rest t) (values particle-3d-updater))))
                scene3d-particle-emitter-spiral-updater))
(defun scene3d-particle-emitter-spiral-updater (target &rest args)
  "A wrapper for PARTICLE-3D-SPIRAL-UPDATER to be passed to MAKE-SCENE3D-PARTICLE-EMITTER."
  (values (apply #'rcurry #'particle-3d-spiral-updater target args)))

(defun make-scene3d-particle-emitter (&key
                                        (updater (scene3d-particle-emitter-billboard-updater))
                                        (capacity +particle-3d-emitter-default-capacity+)
                                        (rate 0.0)
                                        (renderer (particle-3d-cube-renderer))
                                        (position (raylib:vector3-zero))
                                        (scale (raylib:vector3-one))
                                        (rotation (raylib:quaternion-identity))
                                        (color (raylib:copy-color raylib:+white+))
                                        (origin position)
                                        (delta #'game-loop-delta-time))
  "Create a SCENE3D-PARTICLE-EMITTER with CAPACITY, use function UPDATER, which accepts an emission origin and returns a PARTICLE-3D-UPDATER, along with RENDERER of type PARTICLE-3D-RENDERER and construction parameters for SCENE3D-NODEs (POSITION, ORIGIN, SCALE, ROTATION, and COLOR). The number of particles emitted per second is determined by the function RATE (which can be a SINGLE-FLOAT or a function returning a SINGLE-FLOAT) and the interval time returned by function DELTA."
  (let* ((delta (etypecase delta (single-float (constantly delta)) (function delta)))
         (emitter (make-particle-3d-emitter :capacity capacity :updater (funcall updater origin)))
         (updater (particle-3d-emitter-emit-update-draw-function
                   emitter (let ((rate-function (if (functionp rate) rate (constantly rate))))
                             (lambda (&optional (delta (funcall delta)) &aux (count-per-second (funcall rate-function)))
                               (declare (type single-float delta count-per-second))
                               (* delta count-per-second)))
                   (ensure-function renderer)
                   delta)))
    (%make-scene3d-particle-emitter :emitter emitter :updater updater
                                    :position position :scale scale
                                    :rotation rotation :color color)))

(defun scene3d-particle-emitter-burst (emitter count)
  "Make EMITTER emit COUNT particles at once if there is enough remaining capacity."
  (particle-3d-emitter-emit (scene3d-particle-emitter-emitter emitter) count))

(declaim (inline %camera-3d-rotate-around-target))
(defun %camera-3d-rotate-around-target (camera quaternion)
  (clet* ((camera (cthe (:pointer (:struct raylib:camera-3d)) camera))
          (quaternion (cthe (:pointer (:struct raylib:vector4)) quaternion))
          (position (& (-> camera raylib:position)))
          (target (& (-> camera raylib:target)))
          (up (& (-> camera raylib:up)))
          (offset (foreign-alloca '(:struct raylib:vector3))))
    (raylib:%vector3-subtract offset position target)
    (raylib:%vector3-rotate-by-quaternion offset offset quaternion)
    (raylib:%vector3-rotate-by-quaternion up up quaternion)
    (raylib:%vector3-add position target offset)
    nil))

(defmacro %with-camera-rotated-around-target ((camera rotation) &body body)
  (with-gensyms (rotated-camera original-camera)
    `(clet* ((,rotated-camera (cthe (:pointer (:struct raylib:camera-3d)) ,camera))
             (,original-camera ([] ,rotated-camera)))
       (%camera-3d-rotate-around-target ,rotated-camera ,rotation)
       (unwind-protect (progn . ,body)
         (csetf ([] ,rotated-camera) ([] ,original-camera))))))

(defmethod scene3d-draw ((emitter scene3d-particle-emitter) position origin scale rotation tint)
  (clet ((total-rotation (foreign-alloca '(:struct raylib:vector4))))
    (rlgl:push-matrix)
    (raylib:%quaternion-multiply total-rotation (& rotation) (& (scene3d-particle-emitter-rotation emitter)))
    (raylib:%quaternion-invert total-rotation total-rotation)
    (%with-camera-rotated-around-target ((& *scene3d-camera*) total-rotation)
      (rlgl-apply-scene3d-draw-arguments position origin scale rotation tint)
      (rlgl-apply-scene3d-draw-arguments
       (scene3d-particle-emitter-position emitter) (scene3d-particle-emitter-origin emitter) (scene3d-particle-emitter-scale emitter)
       (scene3d-particle-emitter-rotation emitter) (scene3d-particle-emitter-color emitter))
      (clet* ((position (cthe (:pointer (:struct raylib:vector3)) (& (scene3d-particle-emitter-position emitter))))
              (rotation (cthe (:pointer (:struct raylib:vector4)) (& (scene3d-particle-emitter-rotation emitter))))
              (original-position ([] position))
              (inverted-rotation total-rotation))
        (raylib:%quaternion-invert inverted-rotation rotation)
        (raylib:%vector3-rotate-by-quaternion position original-position inverted-rotation)
        (rlgl:translatef (- (-> position raylib:x)) (- (-> position raylib:y)) (- (-> position raylib:z)))
        (funcall (scene3d-particle-emitter-updater emitter))
        (csetf ([] position) ([] original-position))))
    (rlgl:pop-matrix)))

(defun particle-3d-scene3d-particle-emitter-renderer (emitter
                                                      &key
                                                        (position +vector3-zeros+)
                                                        (origin +vector3-zeros+)
                                                        (scale +vector3-ones+)
                                                        (rotation +quaternion-identity+)
                                                        (color raylib:+white+))
  (with-value-generators (emitter position origin scale rotation color)
    (declare (type (function (particle-3d) (values raylib:vector3)) position origin scale)
             (type (function (particle-3d) (values raylib:vector4)) rotation)
             (type (function (particle-3d) (values raylib:color)) color)
             (type (function (particle-3d) (values t)) emitter))
    (let ((emitters (make-hash-table))
          (position-offset position))
      (lambda (particle)
        (let* ((emitter (ensure-gethash particle emitters (funcall emitter particle)))
               (position (scene3d-particle-emitter-position emitter))
               (position-offset (funcall position-offset particle))
               (origin (funcall origin particle))
               (scale (funcall scale particle))
               (rotation (funcall rotation particle))
               (color (funcall color particle)))
          (clet ((particle (cthe (:pointer (:struct particle-3d)) (& particle))))
            (raylib:%vector3-add (& position) (& (-> particle position)) (& position-offset))
            (scene3d-draw emitter eon::+vector3-zeros+ origin scale rotation color)))))))
