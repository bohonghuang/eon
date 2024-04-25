(in-package #:eon.editor)

(defun asset-types ()
  (remove-duplicates
   (loop :for method :in (c2mop:generic-function-methods #'load-asset)
         :for specializer := (first (c2mop:method-specializers method))
         :when (typep specializer 'c2mop:eql-specializer)
           :collect (c2mop:eql-specializer-object specializer))))

(defgeneric edit-asset-form (type source &rest args))

(defun edit-asset (form &optional (types (asset-types)))
  (if form (apply #'edit-asset-form form)
      (with-received-preview-function
        (async
          (let ((type (await (promise-selection "Select the type of the asset." types))))
            (when type
              (with-sent-preview-function ()
                (await (edit-asset-form type nil)))))))))

(defgeneric edit-asset-argument (type argument &optional value))

(defmethod edit-asset-form (type source &rest args)
  #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (flet ((getf-args (indicator &optional default)
           (if (eq indicator :source) source (getf args indicator default)))
         ((setf getf-args) (value indicator)
           (if (eq indicator :source) (setf source value) (setf (getf args indicator) value))))
    (macrolet ((remf-args (indicator)
                 `(if (eq ,indicator :source)
                      (setf source nil)
                      (remf args ,indicator))))
      (with-received-preview-function
        (async
          (do-non-nil (argument (await (promise-selection (format nil "Edit ~A." type)
                                                          (acons :source source
                                                                 (plist-props-alist args (type-construct-arguments type)))))
                                (list* type source args))
            (with-sent-preview-function ((list* type source args) (getf-args argument))
              (if (key-down-p :l3) (remf-args argument)
                  (with-specified-value (value (await (apply #'edit-asset-argument type argument
                                                             (with-specified-value (arg (getf-args argument :unspecified))
                                                               (list arg))))
                                               (remf-args argument))
                    (setf (getf-args argument) value))))))))))

(defmethod edit-asset-argument (type (argument (eql :source)) &optional (value nil valuep))
  (with-received-preview-function
    (async
      (or (selection-case "Select the source of the asset."
            (pathname (with-sent-preview-function ()
                        (return (first (await (promise-dropped-files))))))
            (symbol (with-sent-preview-function ()
                      (return (non-empty-or
                                  (nstring-upcase (await (promise-input-text "Enter a symbol.")))
                                  (specified-value value valuep)
                                  #'intern)))))
          (specified-value value valuep)))))
