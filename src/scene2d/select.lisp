(in-package #:eon)

(defgeneric selectable-container-entries (container)
  (:documentation "Return the entries of CONTAINER."))

(defun selectable-container-p (object)
  (compute-applicable-methods #'selectable-container-entries (list object)))

(deftype selectable-container ()
  "A container that specializes SELECTABLE-CONTAINER-ENTRIES."
  '(satisfies selectable-container-p))

(defgeneric selectable-container-entry-selected-p (entry)
  (:documentation "Return whether ENTRY is focused."))

(defgeneric (setf selectable-container-entry-selected-p) (value entry)
  (:documentation "Set whether ENTRY is focused."))

(defun selectable-container-entry-p (object)
  (and
   (compute-applicable-methods
    #'selectable-container-entry-selected-p
    (list object))
   (compute-applicable-methods
    #'(setf selectable-container-entry-selected-p)
    (list t object))))

(deftype selectable-container-entry ()
  "A SCENE2D-FOCUSABLE that specializes methods SELECTABLE-CONTAINER-ENTRY-SELECTED-P and (SETF SELECTABLE-CONTAINER-ENTRY-SELECTED-P)."
  '(and scene2d-focusable (satisfies selectable-container-entry-p)))

(defmacro await-if-possible (form)
  (once-only (form)
    `(typecase ,form
       (promise:promise (await ,form))
       (t ,form))))

(defun selectable-container-selected-entry (container)
  "Get the currently selected entry of CONTAINER."
  (find-if #'selectable-container-entry-selected-p (selectable-container-entries container)))

(defun (setf selectable-container-selected-entry) (value container)
  "Set the currently selected entry of CONTAINER."
  (loop :for entry :in (selectable-container-entries container)
        :do (setf (selectable-container-entry-selected-p entry) (eq entry value))
        :finally (return value)))

(defun selectable-container-promise-index (container &optional (initial-index 0) (handler (constantly nil)))
  "Allow the user to select a child of CONTAINER using directional buttons and return a PROMISE:PROMISE of the selected child's index. The child with INITIAL-INDEX will be selected by default. HANDLER is called before and after the user presses a button (moves the cursor or makes a selection). Before the button is pressed, it is called with FOCUS-MANAGER as the only parameter. After the button is pressed, it is called with FOCUS-MANAGER and the button pressed by the user as parameters, then if HANDLER returns a non-NIL value, it will be used to fulfill the PROMISE:PROMISE of this function, thereby terminating the user's selection."
  (let* ((entries (selectable-container-entries container))
         (initial-focused (etypecase initial-index
                            (non-negative-fixnum (nth initial-index entries))
                            ((eql nil) (find-if #'selectable-container-entry-selected-p entries))))
         (manager (make-scene2d-focus-manager :focusables (cons initial-focused (delete initial-focused (copy-list entries))))))
    (setf (selectable-container-entry-selected-p initial-focused) t)
    (mapc (curry #'(setf selectable-container-entry-selected-p) nil) (remove initial-focused entries))
    (async
      (loop
        (await-if-possible (funcall handler manager))
        (let ((button (await (promise-pressed-controller-button))))
          (case button
            ((:up :down :left :right)
             (setf (selectable-container-entry-selected-p (scene2d-focus-manager-focused manager)) nil)
             (scene2d-focus-manager-handle-input manager button)))
          (let ((result (await-if-possible (funcall handler manager button))))
            (etypecase result
              (non-negative-fixnum (return result))
              ((eql t) (return nil))
              ((eql nil)
               (case button
                 ((:a) (return (position (scene2d-focus-manager-focused manager) entries)))
                 ((:b) (return nil))))))
          (setf (selectable-container-entry-selected-p (scene2d-focus-manager-focused manager)) t))))))

(defun selectable-container-entry-content (instance)
  (scene2d-focusable-content instance))

(defun (setf selectable-container-entry-content) (value instance)
  (setf (scene2d-focusable-content instance) value))
