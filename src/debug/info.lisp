(in-package #:eon.debug)

(defun runtime-information ()
  (loop :with stream := (make-string-output-stream)
        :for (info-form . rest) :on '(raylib:+raylib-version+
                                      (lisp-implementation-type) (lisp-implementation-version)
                                      (machine-type) (machine-version) (machine-instance)
                                      (software-type) (software-version))
        :for info-value := (prin1-to-string (eval info-form))
        :do (format stream "~A~A=> ~A" info-form (if (> (length info-value) 24) #\Newline #\Space) info-value)
        :when rest
          :do (format stream "~%")
        :finally (return (get-output-stream-string stream))))
