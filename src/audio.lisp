(in-package #:eon)

(deftype audio-sample-fetcher ()
  "A function that takes a RAYLIB:AUDIO-STREAM and its required number of audio samples to generate and update the samples for that audio stream."
  `(function (raylib:audio-stream non-negative-fixnum) (values boolean)))

(defun audio-sample-fetcher-subseq (sample-fetcher start &optional end)
  "Slice SAMPLE-FETCHER to the sample range specified by START and END, and return a new proxy AUDIO-SAMPLE-FETCHER."
  (let ((fetched-sample-count 0))
    (lambda (audio-stream sample-count)
      (when (< fetched-sample-count end)
        (when (< fetched-sample-count start)
          (funcall sample-fetcher audio-stream start)
          (incf fetched-sample-count start))
        (when end (minf sample-count (- end fetched-sample-count)))
        (prog1 (funcall sample-fetcher audio-stream sample-count)
          (incf fetched-sample-count sample-count))))))

(deftype audio ()
  "A generic audio type."
  `(or raylib:audio-stream raylib:music raylib:sound))

(defmacro define-audio-parameter (parameter &optional (default 1.0))
  (let* ((accessor (symbolicate '#:audio- parameter))
         (store (symbolicate '* accessor '#:s '*)))
    (with-gensyms (sound music stream audio)
      (let ((*package* (find-package '#:raylib)))
        `(progn
           (defvar ,store (tg:make-weak-hash-table :weakness :key :test #'eq))
           (defgeneric ,accessor (,audio))
           (defmethod ,accessor (,audio)
             (ensure-gethash ,audio ,store ,default))
           (defgeneric (setf ,accessor) (,parameter ,audio))
           (defmethod (setf ,accessor) (,parameter (,sound raylib:sound))
             (,(symbolicate '#:set-sound- parameter) ,sound (setf (gethash ,sound ,store) ,parameter)))
           (defmethod (setf ,accessor) (,parameter (,music raylib:music))
             (,(symbolicate '#:set-music- parameter) ,music (setf (gethash ,music ,store) ,parameter)))
           (defmethod (setf ,accessor) (,parameter (,stream raylib:audio-stream))
             (,(symbolicate '#:set-audio-stream- parameter) ,stream (setf (gethash ,stream ,store) ,parameter))))))))

(define-audio-parameter volume 1.0)
(define-audio-parameter pan 0.0)
(define-audio-parameter pitch 1.0)

(defgeneric play-audio (audio)
  (:method ((sound raylib:sound))
    (values
     sound
     (with-promise (succeed)
       (raylib:play-sound sound)
       (add-game-loop-hook
        (lambda () (if (raylib:is-sound-playing sound) t (progn (succeed) nil)))
        :after #'identity))))
  (:method ((music raylib:music))
    (values
     music
     (with-promise (succeed)
       (raylib:play-music-stream music)
       (add-game-loop-hook
        (lambda () (if (raylib:is-music-stream-playing music) (progn (raylib:update-music-stream music) t) (progn (succeed) nil)))
        :after #'identity))))
  (:method ((stream raylib:audio-stream))
    (values
     stream
     (with-promise (succeed)
       (raylib:play-audio-stream stream)
       (add-game-loop-hook
        (lambda () (if (raylib:is-audio-stream-playing stream) t (progn (succeed) nil)))
        :after #'identity))))
  (:documentation "Play AUDIO and return it."))

(defgeneric pause-audio (audio)
  (:method ((sound raylib:sound)) (raylib:pause-sound sound))
  (:method ((music raylib:music)) (raylib:pause-music-stream music))
  (:method ((stream raylib:audio-stream)) (raylib:pause-audio-stream stream))
  (:documentation "Pause AUDIO."))

(defgeneric resume-audio (audio)
  (:method ((sound raylib:sound)) (raylib:resume-sound sound))
  (:method ((music raylib:music)) (raylib:resume-music-stream music))
  (:method ((stream raylib:audio-stream)) (raylib:resume-audio-stream stream))
  (:documentation "Resume AUDIO."))

(defgeneric stop-audio (audio)
  (:method ((sound raylib:sound)) (raylib:stop-sound sound))
  (:method ((music raylib:music)) (raylib:stop-music-stream music))
  (:method ((stream raylib:audio-stream)) (raylib:stop-audio-stream stream))
  (:documentation "Stop AUDIO."))

(defgeneric audio-playing-p (audio)
  (:method ((sound raylib:sound)) (raylib:is-sound-playing sound))
  (:method ((music raylib:music)) (raylib:is-music-stream-playing music))
  (:method ((stream raylib:audio-stream)) (raylib:is-audio-stream-playing stream))
  (:documentation "Return whether AUDIO is playing."))

(defvar *audio-paused-p-table* (tg:make-weak-hash-table :weakness :key :test #'eq))

(defun audio-paused-p (audio)
  "Return whether AUDIO is paused."
  (values (gethash audio *audio-paused-p-table*)))

(defmethod play-audio :after (audio)
  (remhash audio *audio-paused-p-table*))

(defmethod play-audio :around (audio)
  (multiple-value-bind (audio promise) (call-next-method)
    (log:trace "Playing audio: ~S" audio)
    (when (log:trace)
      (async
        (await promise)
        (log:trace "Finish playing audio: ~S" audio)))
    (values audio promise)))

(defmethod stop-audio :after (audio)
  (remhash audio *audio-paused-p-table*)
  (log:trace "Stopped audio: ~S" audio))

(defmethod pause-audio :after (audio)
  (setf (gethash audio *audio-paused-p-table*) t)
  (log:trace "Paused audio: ~S" audio))

(defmethod resume-audio :after (audio)
  (remhash audio *audio-paused-p-table*)
  (log:trace "Resumed audio: ~S" audio))

(defun (setf audio-paused-p) (value audio)
  (unless (eq value (audio-paused-p audio))
    (if value (pause-audio audio) (resume-audio audio))))

(defmethod play-audio :before (audio)
  (assert (not (audio-playing-p audio))))

(defmethod stop-audio :before (audio)
  (assert (audio-playing-p audio)))

(defmethod pause-audio :before (audio)
  (assert (audio-playing-p audio))
  (assert (not (audio-paused-p audio))))

(defmethod resume-audio :before (audio)
  (assert (not (audio-playing-p audio)))
  (assert (audio-paused-p audio)))

(defconstant +audio-stream-buffer-size-default+ (truncate 48000 30))

(defconstant +audio-stream-pool-enabled-p+ t)

(defun play-audio-load-audio-stream ()
  (load-asset 'raylib:audio-stream nil))

(defun play-audio-unload-audio-stream (stream)
  (unload-asset stream))

(declaim (special *audio-stream-pool*))
(when +audio-stream-pool-enabled-p+
  (setf (assoc-value *game-special-bindings* '*audio-stream-pool*)
        `(progn (raylib:set-audio-stream-buffer-size-default ,+audio-stream-buffer-size-default+) nil))
  (defun pool-audio-stream (stream)
    (raylib:stop-audio-stream stream)
    (push stream *audio-stream-pool*)
    (log:trace "Pooled audio stream: ~S" stream))
  (defun unpool-audio-stream ()
    (if-let ((stream (pop *audio-stream-pool*)))
      (progn
        (log:trace "Unpooled audio stream: ~S" stream)
        (setf (audio-volume stream) 1.0
              (audio-pan stream) 0.0
              (audio-pitch stream) 1.0)
        stream)
      (let ((stream (load-asset 'raylib:audio-stream nil)))
        (log:trace "Loaded new audio stream: ~S" stream)
        stream)))
  (setf (fdefinition 'play-audio-unload-audio-stream) (fdefinition 'pool-audio-stream)
        (fdefinition 'play-audio-load-audio-stream) (fdefinition 'unpool-audio-stream)))

(defmethod audio-playing-p ((sample-fetcher function)) nil)

(defmethod play-audio ((sample-fetcher function #| audio-sample-fetcher |#))
  (declare (type audio-sample-fetcher sample-fetcher) #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (unless +audio-stream-pool-enabled-p+
    (raylib:set-audio-stream-buffer-size-default +audio-stream-buffer-size-default+))
  (let ((stream (play-audio-load-audio-stream))
        (sample-count +audio-stream-buffer-size-default+))
    (values
     stream
     (with-promise (succeed)
       (raylib:play-audio-stream stream)
       (add-game-loop-hook
        (flet ((finish-playback () (play-audio-unload-audio-stream stream) (succeed) nil))
          (lambda ()
            (and (or (raylib:is-audio-stream-playing stream)
                     ;; FIXME: If stopping and playing are performed simultaneously within the same frame,
                     ;;        the reuse of RAYLIB:AUDIO-STREAM may result in (FINISH-PLAYBACK) not being called.
                     (audio-paused-p stream)
                     (finish-playback))
                 (or (not (raylib:is-audio-stream-processed stream))
                     (funcall sample-fetcher stream sample-count)
                     (finish-playback)))))
        :after #'identity)))))

(defun promise-play-audio (audio)
  "Play AUDIO and return a PROMISE which is fulfilled when the playback is finished."
  (multiple-value-bind (audio promise) (play-audio audio)
    (values promise audio)))

(defun fade-audio (audio volume &optional (duration 0.5))
  "Fade the volume of AUDIO to VOLUME within DURATION."
  (log:trace "Fading volume to ~,2F in ~,2F seconds for audio ~S" volume duration audio)
  (ute:start (ute:tween :to (((audio-volume audio)) (volume))
                        :ease #'ute:linear-inout
                        :duration duration)))

(defun promise-fade-audio (audio volume &optional (duration 0.5))
  "Like FADE-AUDIO, but return a PROMISE which is fulfilled when the fading is finished."
  (with-promise (succeed)
    (if (plusp duration)
        (let* ((tween (fade-audio audio volume duration))
               (callback (ute:callback tween)))
          (setf (ute:callback tween) (lambda () (funcall callback) (succeed audio))))
        (progn (setf (audio-volume audio) volume) (succeed audio)))))

(defun crossfade-audio (from to &optional (duration-out 1.0) (duration-in 0.0))
  "Fade out audio FROM within DURATION-OUT and fade in audio TO within DURATION-IN."
  (multiple-value-bind (to promise) (play-audio to)
    (pause-audio to)
    (values
     to
     (async
       (log:trace "Fading out audio: ~S" from)
       (await (promise-fade-audio from 0.0 duration-out))
       (when (audio-playing-p from)
         (stop-audio from))
       (when (audio-paused-p to)
         (resume-audio to)
         (when (plusp duration-in)
           (setf (audio-volume to) 0.0)
           (log:trace "Fading in audio: ~S" from)
           (await (promise-fade-audio to 1.0 duration-in))))
       to)
     promise)))

(defun promise-crossfade-audio (from to &optional (duration-out 1.0) (duration-in 0.0))
  "Like CROSSFADE-AUDIO, but return a PROMISE which is fulfilled when the crossfading is finished."
  (multiple-value-bind (audio promise-crossfade-finish promise-playback-finish)
      (crossfade-audio from to duration-out duration-in)
    (values promise-crossfade-finish promise-playback-finish audio)))
