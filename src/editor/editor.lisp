(in-package #:eon.editor)

(defmacro with-editor-setup (draw &body body)
  `(let ((*debug-window-group* (scene2d-construct (scene2d-group))))
     (flet ((,draw ()
              (unless (key-down-p :l2)
                (scene2d-draw-simple *debug-window-group*))))
       . ,body)))

(cobj:define-global-cobject +background-color+ (raylib:make-color :r 140 :g 150 :b 255 :a 255))

(defmacro define-standalone-editor (name &optional (edit (symbolicate '#:edit- (make-symbol (subseq (symbol-name name) 0 (search "-EDITOR" (symbol-name name)))))) default-form)
  (with-gensyms (form)
    `(defun ,name (&optional (,form ,default-form))
       (let ((title (format nil "Pokémon: Eonian Emerald - ~A" ',name)))
         (unwind-protect
              (raylib:with-window (title ((* +world-viewport-default-width+ 2) (* +world-viewport-default-height+ 2)))
                (raylib:set-target-fps 60)
                (raylib:set-window-min-size +world-viewport-default-width+ +world-viewport-default-height+)
                (with-game-context
                  (let ((viewport (make-fit-viewport)))
                    (with-editor-setup draw-editor
                      (async
                        (loop :do (setf ,form (await (,edit ,form)))
                              :until (await (promise-yes-or-no-p "Do you want to exit?"))
                              :finally (return-from ,name ,form)))
                      (do-game-loop
                        (raylib:with-drawing
                          (with-viewport viewport
                            (raylib:clear-background +background-color+)
                            (draw-editor))))))))
           (return-from ,name ,form))))))
