(defpackage eon
  (:use #:cl #:alexandria #:cffi #:cffi-ops #:promise-async-await)
  (:export
   ;; viewport.lisp
   #:viewport
   #:begin-viewport
   #:end-viewport
   #:draw-viewport
   #:viewport-width
   #:viewport-height
   #:with-viewport
   #:screen-viewport
   #:make-screen-viewport
   #:+world-viewport-default-width+
   #:+world-viewport-default-height+
   #:stretch-viewport
   #:make-stretch-viewport
   #:fit-viewport
   #:make-fit-viewport
   ;; asset.lisp
   #:load-asset
   #:unload-asset
   #:asset-loaded-p
   ;; loop.lisp
   #:game-loop-delta-time
   #:game-loop-time
   #:add-game-loop-hook
   #:remove-game-loop-hook
   #:do-game-loop
   #:promise-sleep
   #:with-game-context
   #:game-loop-once-only
   #:promise-task
   ;; tween.lisp
   #:promise-tween
   ;; input.lisp
   #:*keyboard-key-mappings*
   #:controller-button-pressed-p
   #:controller-button-down-p
   #:controller-button-released-p
   #:controller-button-up-p
   #:pressed-controller-button
   #:promise-pressed-controller-button
   #:*controller-button-queue*
   ;; texture.lisp
   #:texture-region
   #:make-texture-region
   #:texture-region-region
   #:texture-region-texture
   #:texture-region-width
   #:texture-region-height
   #:split-texture
   ;; misc.lisp
   #:n-patch
   #:make-n-patch
   #:text
   #:make-text
   #:text-style
   ;; scene2d/basic.lisp
   #:scene2d-draw
   #:scene2d-draw-simple
   #:scene2d-size
   #:scene2d-layout
   #:scene2d-node
   #:scene2d-position
   #:scene2d-origin
   #:scene2d-scale
   #:scene2d-color
   #:scene2d-rotation
   #:ensure-scene2d-node
   #:scene2d-container
   #:scene2d-alignment
   #:scene2d-cell
   #:scene2d-margin
   #:scene2d-box
   #:scene2d-box-children
   #:scene2d-box-add-child
   #:scene2d-box-remove-child
   #:scene2d-nine-patch
   #:scene2d-window-style
   #:scene2d-window
   #:scene2d-window-child
   #:scene2d-window-background
   #:scene2d-window-layout
   #:scene2d-flow-box
   #:scene2d-flow-box-children
   #:scene2d-flow-box-add-child
   #:scene2d-coordinate-truncator
   #:scene2d-label-style
   #:scene2d-label
   #:scene2d-label-string
   #:scene2d-scissor
   #:scene2d-image
   #:scene2d-image-tween-frames
   #:scene2d-group
   #:scene2d-group-children
   #:scene2d-group-add-child
   #:scene2d-group-remove-child
   #:scene2d-dimensions
   #:scene2d-max-cell
   #:scene2d-table
   #:scene2d-table-newline
   #:scene2d-table-add-child
   #:scene2d-table-children
   #:scene2d-shaderable-container
   #:scene2d-canvas
   #:scene2d-tween-container
   #:scene2d-tween-container-manager
   #:scene2d-tween-container-speed
   #:scene2d-rectangle
   ;; scene2d/construct.lisp
   #:scene2d-construct-form
   #:scene2d-construct
   #:define-scene2d-default-construct-form
   #:define-scene2d-constructed
   #:scene2d-constructed
   #:scene2d-constructed-metadata
   ;; scene2d/focus.lisp
   #:scene2d-focusable
   #:scene2d-focus-manager
   #:make-scene2d-focus-manager
   #:scene2d-focus-manager-focused
   #:scene2d-focus-manager-handle-input
   ;; scene2d/scroll.lisp
   #:scene2d-scroll-region
   #:scene2d-scroll-region-scroll-to-focusable
   #:scene2d-scroll-region-child
   #:scene2d-box-scroll-region
   #:scene2d-tile-scroll-style
   #:scene2d-tile-scroll-region-style
   #:scene2d-tile-scroll-region
   #:scene2d-tile-scroll-region-offset
   ;; scene2d/ui/select.lisp
   #:select-box
   #:select-box-style
   #:select-box-entries
   #:select-box-add-child
   #:select-box-children
   #:select-box-entry-content
   #:select-box-entry-focused-p
   #:select-box-promise-index
   #:swappable-select-box
   #:swappable-select-box-promise-index
   #:table-select-box
   #:select-box-border-entry
   #:select-box-transparent-entry
   ;; scene2d/ui/input.lisp
   #:input-field
   #:input-field-style
   #:input-field-label
   #:input-field-cursor
   #:input-field-promise-line
   #:input-field-text
   ;; scene2d/ui/dialog.lisp
   #:dialog-box-text
   #:dialog-box-text-style
   #:dialog-box-text-string
   #:dialog-box
   #:dialog-box-string
   #:dialog-box-style
   #:dialog-box-promise-display
   #:dialog-box-promise-confirm
   #:dialog-box-promise-display-confirm
   #:ensure-dialog-box-text-label
   #:promise-display-dialog-box-text-label
   ;; utils.lisp
   #:integer-float
   #:array-vector
   ;; particle.lisp
   #:particle-3d
   #:particle-3d-position
   #:particle-3d-velocity
   #:particle-3d-acceleration
   #:particle-3d-rotation
   #:particle-3d-rotation-acceleration
   #:particle-3d-rotation-velocity
   #:particle-3d-age
   #:particle-3d-lifetime
   #:particle-3d-livep
   #:particle-3d-initialize-default
   #:particle-3d-update-motion
   #:particle-3d-emitter
   #:make-particle-3d-emitter
   #:particle-3d-emitter-update
   #:particle-3d-emitter-draw
   #:particle-3d-emitter-emit-update-draw-function
   #:particle-3d-emitter-emit
   #:make-particle-3d-vector2-generator
   #:derive-particle-3d-vector2-generator
   #:particle-3d-billboard-updater
   #:particle-3d-laser-updater
   #:particle-3d-spiral-updater
   #:particle-3d-renderer
   #:particle-3d-cube-renderer
   #:particle-3d-sphere-renderer
   #:particle-3d-interpolate-color-over-age
   #:particle-3d-interpolate-vector3-over-age
   #:particle-3d-interpolate-quaternion-over-age
   #:particle-3d-iterate-sequence-over-age
   ;; scene3d/basic.lisp
   #:scene3d-node
   #:scene3d-position
   #:scene3d-scale
   #:scene3d-rotation
   #:scene3d-color
   #:scene3d-draw
   #:scene3d-draw-simple
   #:scene3d-bound
   #:scene3d-layout
   #:ensure-scene3d-node
   #:*scene3d-camera*
   #:scene3d-container
   #:scene3d-container-content
   #:make-scene3d-container
   #:scene3d-billboard
   #:make-scene3d-billboard
   #:scene3d-billboard-tween-frames
   #:scene3d-alignment
   #:make-scene3d-alignment
   #:scene3d-cell
   #:make-scene3d-cell
   #:scene3d-canvas
   #:make-scene3d-canvas
   #:scene3d-shaderable-container
   #:make-scene3d-shaderable-container
   ;; scene3d/particle.lisp
   #:scene3d-particle-emitter
   #:particle-3d-scene3d-node-renderer
   #:particle-3d-scene3d-billboard-renderer
   #:particle-3d-scene3d-bullet-renderer
   #:particle-3d-scene3d-node-sorting-renderer
   #:particle-3d-scene3d-particle-emitter-renderer
   #:scene3d-particle-emitter-billboard-updater
   #:scene3d-particle-emitter-laser-updater
   #:scene3d-particle-emitter-spiral-updater
   #:make-scene3d-particle-emitter
   #:scene3d-particle-emitter-burst
   ;; shader.lisp
   #:define-shaderable-uniforms
   ;; tiled.lisp
   #:*tiled-renderer-camera*
   #:tiled-renderer
   #:tiled-layer-renderer
   #:tiled-map-renderer
   ;; audio.lisp
   #:audio
   #:audio-sample-fetcher
   #:audio-sample-fetcher-subseq
   #:play-audio
   #:promise-play-audio
   #:stop-audio
   #:pause-audio
   #:resume-audio
   #:audio-volume
   #:audio-pan
   #:audio-pitch
   #:audio-playing-p
   #:audio-paused-p
   #:fade-audio
   #:promise-fade-audio
   #:crossfade-audio
   #:promise-crossfade-audio
   ;; screen.lisp
   #:with-screen-manager-mode
   #:screen-manager
   #:current-screen
   #:take-screenshot
   #:do-screen-loop
   #:screen-render
   #:define-simple-shader-screen-transition
   #:make-screen-transition
   #:play-screen-transition
   #:promise-play-screen-transition
   #:transition-screen
   #:promise-transition-screen
   #:screen-transition-fade
   ;; shadow.lisp
   #:shadow-map-renderer
   #:make-shadow-map-renderer
   #:shadow-map-renderer-matrix
   #:shadow-map-renderer-texture
   #:shadow-map-renderer-render))

(in-package #:eon)

(rename-package '#:org.shirakumo.promise '#:org.shirakumo.promise '(#:promise))
(rename-package '#:cl-tiled '#:cl-tiled '(#:tiled))
