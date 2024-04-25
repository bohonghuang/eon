(in-package #:eon.debug)

(declaim (special *debug-window-group*))

(defgeneric brief-string (object)
  (:method (object) (princ-to-string object))
  (:method ((symbol symbol)) (symbol-name symbol))
  (:method ((list list))
    (cond
      ((eq (first list) 'quote) (brief-string (second list)))
      ((and (loop :for (prop . value) :on (cdr list) :by #'cddr
                  :always (and (keywordp prop) value))
            (car list))
       (brief-string (car list)))
      ((every #'listp list) (format nil "(~D item~:P)" (length list)))
      (t (princ-to-string (mapcar #'brief-string list)))))
  (:method ((pathname pathname))
    (format nil "~A~@[.~A~]" (pathname-name pathname) (pathname-type pathname))))

(defun margin-all (child &optional (margin 1.0))
  (scene2d-construct (scene2d-margin :top margin :bottom margin :left margin :right margin :child child)))

(defun list-select-box (selections)
  (loop :with select-box := (scene2d-construct (select-box))
        :for selection :in selections
        :do (select-box-add-child select-box (margin-all (scene2d-construct (scene2d-label :string (brief-string selection) :style (scene2d-label-style)))))
        :finally (return select-box)))

(defun alist-table-select-box (selections)
  (loop :with table := (scene2d-construct (scene2d-table))
        :for (key . value) :in selections
        :do (scene2d-table-newline table)
            (setf (eon::scene2d-cell-alignment
                   (scene2d-table-add-child table (margin-all (scene2d-construct (scene2d-margin :right 4.0 :child (scene2d-label :string (brief-string key) :style (scene2d-label-style)))))))
                  (eon::make-scene2d-alignment :vertical :center :horizontal :start)
                  (eon::scene2d-cell-alignment
                   (scene2d-table-add-child table (margin-all (scene2d-construct (scene2d-margin :left 4.0 :child (scene2d-label :string (brief-string value) :style (scene2d-label-style)))))))
                  (eon::make-scene2d-alignment :vertical :center :horizontal :end))
        :finally (return (table-select-box table))))

(defmacro with-popped-window ((child &optional (alignment '(:start :start))) &body body)
  (with-gensyms (cell)
    (destructuring-bind (&optional (halign :start) (valign :start)) alignment
      (setf body `(let ((,cell (screen-cell (scene2d-construct (scene2d-coordinate-truncator :child (scene2d-window :child ,child))) ,halign ,valign)))
                    (scene2d-group-add-child *debug-window-group* ,cell)
                    (scene2d-layout *debug-window-group*)
                    (prog1 (progn . ,body) (scene2d-group-remove-child *debug-window-group* ,cell)))))))

(defmacro with-popped-prompt (prompt &body body)
  `(with-popped-window ((margin-all (scene2d-construct (scene2d-label :string ,prompt))) (:start :end)) . ,body))

(defun screen-cell (child &optional (halign :start) (valign :start))
  (scene2d-construct
   (scene2d-cell
    :child (scene2d-coordinate-truncator :child child)
    :alignment (eon::make-scene2d-alignment :vertical valign :horizontal halign)
    :size (#.+world-viewport-default-width+ #.+world-viewport-default-height+))))
