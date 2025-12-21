(in-package #:eon)

(defmacro with-begin-matrix-mode (&body body)
  `(prog2 (rlgl:push-matrix) (progn . ,body) (rlgl:push-matrix) (rlgl:load-identity)))

(defmacro with-end-matrix-mode (&body body)
  `(prog2 (rlgl:pop-matrix) (progn . ,body) (rlgl:push-matrix) (rlgl:pop-matrix) (rlgl:pop-matrix)))

(defmacro with-render-texture (target &body body)
  "Like RAYLIB:WITH-TEXTURE-MODE, but protect and reset the matrix stack."
  `(progn
     (with-begin-matrix-mode (raylib:begin-texture-mode ,target))
     (unwind-protect (progn . ,body)
       (with-end-matrix-mode (raylib:end-texture-mode)))))

(defmacro with-camera-2d (camera &body body)
  "Like RAYLIB:WITH-MODE-2D, but protect and reset the matrix stack."
  `(raylib:with-mode-2d ,camera . ,body))

(defmacro with-camera-3d (camera &body body)
  "Like RAYLIB:WITH-MODE-3D, but protect and reset the matrix stack."
  `(progn
     (with-begin-matrix-mode (raylib:begin-mode-3d ,camera))
     (unwind-protect (progn . ,body)
       (with-end-matrix-mode (raylib:end-mode-3d)))))
