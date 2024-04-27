(in-package #:eon.debug)

(defun promise-selection (prompt selections &optional (initial-selection nil initial-selection-p) swappablep)
  (let* ((select-box-type (if (every (conjoin #'consp (disjoin (complement #'proper-list-p)
                                                               (compose #'keywordp #'car)))
                                     selections)
                              'alist-table-select-box 'list-select-box))
         (select-box (funcall (if swappablep #'swappable-select-box #'identity)
                              (funcall select-box-type selections)))
         (promise-index (if swappablep #'swappable-select-box-promise-index #'select-box-promise-index))
         (region (scene2d-box-scroll-region select-box 13)))
    (async
      (with-popped-prompt prompt
        (with-popped-window (region (:start :start))
          (when-let ((index (await (funcall promise-index select-box
                                            (or (and initial-selection-p (position initial-selection selections)) 0)
                                            (let ((initializedp nil))
                                              (lambda (manager &optional key)
                                                (unless key
                                                  (let ((function (curry #'scene2d-scroll-region-scroll-to-focusable
                                                                         region (scene2d-focus-manager-focused manager))))
                                                    (if initializedp
                                                        (funcall function)
                                                        (add-game-loop-hook (curry #'add-game-loop-hook function :after nil) :before nil))
                                                    (setf initializedp t)))))))))
            (funcall
             (ecase select-box-type
               (alist-table-select-box #'car)
               (list-select-box #'identity))
             (etypecase index
               (integer (elt selections index))
               (cons (cons (elt selections (car index))
                           (elt selections (cdr index))))))))))))

(defun promise-input-text (prompt &optional (initial-text ""))
  (let* ((input-field (scene2d-construct (input-field :string initial-text)))
         (previous-text initial-text))
    (add-game-loop-hook
     (lambda ()
       (when previous-text
         (unless (eq (input-field-text input-field) previous-text)
           (setf previous-text (input-field-text input-field))
           (scene2d-layout *debug-window-group*))))
     :before (lambda (res) (declare (ignore res)) previous-text))
    (async
      (with-popped-prompt prompt
        (with-popped-window ((margin-all input-field) (:center :center))
          (prog1 (await (input-field-promise-line input-field))
            (setf previous-text nil)))))))

(defun promise-yes-or-no-p (prompt)
  (async (eql (await (promise-selection prompt '(yes no))) 'yes)))

(defun promise-hint (text &optional (confirm 1))
  (async
    (with-popped-window ((margin-all (scene2d-construct (scene2d-label :string text))) (:center :center))
      (typecase confirm
        (real (await (promise-sleep confirm)))
        (boolean (when confirm (await (promise-pressed-key)))))
      nil)))

(defmacro do-non-nil ((var next &optional result) &body body)
  `(loop :for ,var := ,next
         :while ,var
         :do (progn . ,body)
         ,@(when result `(:finally (return ,result)))))

(defmacro selection-case (prompt &body cases)
  (with-gensyms (result previous-selection null)
    `(let ((,previous-selection ',null))
       (do-non-nil (,result (setf ,previous-selection
                                  (await (apply #'promise-selection (string ,prompt) ',(mapcan (compose #'copy-list #'ensure-list #'car) cases)
                                                (case ,previous-selection (,null nil) (t (list ,previous-selection)))))))
         (case ,result . ,cases)))))

(defun promise-dropped-files ()
  (let ((promise (promise:with-promise (succeed)
                   (let ((path-list (raylib:make-file-path-list)))
                     (add-game-loop-hook
                      (lambda ()
                        (if (key-pressed-p :b) (succeed nil)
                            (progn
                              (raylib:%load-dropped-files (& path-list))
                              (unwind-protect
                                   (when (plusp (raylib:file-path-list-count path-list))
                                     (succeed
                                      (mapcar
                                       (lambda (pointer)
                                         (pathname (the string (cobj:cref pointer))))
                                       (cobj:ccoerce
                                        (cobj:pointer-carray
                                         (clet ((path-list (cthe (:pointer (:struct raylib:file-path-list)) (& path-list))))
                                           (& (-> path-list raylib:paths)))
                                         '(cobj:cpointer string)
                                         (raylib:file-path-list-count path-list))
                                        'list))))
                                (raylib:%unload-dropped-files (& path-list))))))
                      :before #'not)))))
    (async
      (with-popped-prompt "Waiting for dropped files..."
        (await promise)))))
