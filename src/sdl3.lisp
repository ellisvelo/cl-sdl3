(in-package #:sdl3)

(define-condition sdl-error (error)
  ((string :initarg :string :initform nil :accessor sdl-error-string))
  (:report (lambda (c s)
             (with-slots (string) c
               (format s "SDL Error: ~A" string)))))

(define-condition sdl-rc-error (sdl-error)
  ((code :initarg :rc :initform nil :accessor sdl-error-code))
  (:report (lambda (c s)
             (with-slots (code string) c
               (format s "SDL Error (~A): ~A" code string)))))

(define-condition sdl-continue (condition) ())
(define-condition sdl-quit (condition) ())

(defun sdl-true-p (integer-bool)
  "Use this function to convert truth from a low level wrapped SDL function
returning an SDL_true into CL's boolean type system."
  (= 1 integer-bool))

(defun boolean-to-bool (bool)
  "Convert a Lisp boolean into a C bool."
  (if bool
      1
      0))

(autowrap:define-bitmask-from-constants (sdl-init-flags)
  sdl3-ffi:+sdl-init-audio+
  sdl3-ffi:+sdl-init-video+
  sdl3-ffi:+sdl-init-joystick+
  sdl3-ffi:+sdl-init-haptic+
  sdl3-ffi:+sdl-init-gamepad+
  sdl3-ffi:+sdl-init-events+
  sdl3-ffi:+sdl-init-sensor+
  sdl3-ffi:+sdl-init-camera+
'(:everything . #x0000FFFF))

;;; NAMING CONVENTION: check-<foo>
;;; If <foo> names a specific value (true, false, zero, null, etc),
;;; check-<foo> shall error `(when <foo> ...)`.  E.g., `(check-false
;;; x)` will *error* when `x` is false.
;;; If <foo> names something that can have an error state (like a
;;; return code), `(check-<foo> x)` shall error when `x` is in that
;;; state.

(defmacro check-rc (form)
  (with-gensyms (rc)
    `(let ((,rc ,form))
       (declare (type fixnum ,rc))
       (when (minusp ,rc)
         (error 'sdl-rc-error :rc ,rc :string (sdl-get-error)))
       ,rc)))

(defmacro check-zero (form)
  (with-gensyms (rc)
    `(let ((,rc ,form))
       (declare (type fixnum ,rc))
       (when (zerop ,rc)
         (error 'sdl-rc-error :rc ,rc :string (sdl-get-error)))
       ,rc)))

(defmacro check-false (form)
  (with-gensyms (rc)
    `(let ((,rc ,form))
       (declare (type fixnum ,rc))
       (when (not (sdl-true-p ,rc))
         (error 'sdl-rc-error :rc ,rc :string (sdl-get-error)))
       ,rc)))

(defmacro check-nullptr (form)
  (with-gensyms (wrapper)
    `(let ((,wrapper ,form))
       (declare (type autowrap:wrapper ,wrapper))
       (if (null-pointer-p (autowrap:ptr ,wrapper))
           (error 'sdl-error :string (sdl-get-error))
           ,wrapper))))

(defmacro check-nil (form)
  (with-gensyms (v)
    `(let ((,v ,form))
       (if (null ,v)
           (error 'sdl-error :string (sdl-get-error))
           ,v))))

(defvar *the-main-thread* nil)
(defvar *main-thread-channel* nil)
(defvar *main-thread* nil)
(defvar *lisp-message-event* nil)
(defvar *wakeup-event* nil)

(defmacro in-main-thread ((&key background no-event) &body b)
  (with-gensyms (fun channel)
    `(let ((,fun (lambda () ,@b)))
       (if (or *main-thread-channel* *main-thread*)
           (if *main-thread*
               (funcall ,fun)
               ,(if background
                    `(progn
                       (sendmsg *main-thread-channel* (cons ,fun nil))
                       (values))
                    `(let ((,channel (make-channel)))
                       (sendmsg *main-thread-channel* (cons ,fun ,channel))
                       ,(unless no-event
                          '(push-event *wakeup-event*))
                       (let ((result (recvmsg ,channel)))
                         (etypecase result
                           (list (values-list result))
                           (error (error result)))))))
           (error "No main thread, did you call SDL_Init?")))))

(defun handle-message (msg)
  (let ((fun (car msg))
        (chan (cdr msg))
        (condition))
    (handler-bind ((sdl-continue
                     (lambda (c)
                       (declare (ignore c))
                       (when chan (sendmsg chan nil))
                       (return-from handle-message)))
                   (sdl-quit
                     (lambda (c)
                       (declare (ignore c))
                       (when chan (sendmsg chan nil))
                       (quit)
                       (return-from handle-message))))
      (handler-bind ((error (lambda (e) (setf condition e))))
        (if chan
            (sendmsg chan (multiple-value-list (funcall fun)))
            (funcall fun))))))

(defun recv-and-handle-message ()
  (let ((msg (recvmsg *main-thread-channel*)))
    (handle-message msg)))

(defun get-and-handle-messages ()
  (loop :as msg = (and *main-thread-channel*
                       (getmsg *main-thread-channel*))
        :while msg :do
          (handle-message msg)))

(defun sdl-main-thread ()
  (float-features:with-float-traps-masked t
    (let ((*main-thread* (bt:current-thread)))
      (loop :while *main-thread-channel* :do
        (block loop-block
          (restart-bind ((continue (lambda (&optional v)
                                     (declare (ignore v))
                                     (signal 'sdl-continue))
                                   :report-function
                                   (lambda (stream)
                                     (format stream "Return to the SDL3 main loop.")))
                         (abort (lambda (&optional v)
                                  (declare (ignore v))
                                  (signal 'sdl-quit))
                                :report-function
                                (lambda (stream)
                                  (format stream "Abort, quitting SDL3 entirely."))))
            (recv-and-handle-message)))))))

(defun ensure-main-channel ()
  (unless *main-thread-channel*
    (setf *main-thread-channel* (make-channel))))

(defun make-this-thread-main (&optional function)
  "Designate the current thread as the SDL3 main thread. This function will not return until
`SDL3:QUIT` is handled. Users of this function will need to start other threads before this call, or
specify `FUNCTION`.

If `FUNCTION` is specified, it will be called when the main thread channel is ensured. This is like
calling `IN-MAIN-THREAD`, except it allows for a potentially single-threaded application. This
function does **not** return just because `FUNCTION` returns; it still requires `SDL3:QUIT` be
processed.

This does **not** call `SDL3:INIT` by itself. Do this either with `FUNCTION`, or from a separate
thread."
  (ensure-main-channel)
  (when (functionp function)
    (sendmsg *main-thread-channel* (cons function nil)))
  (sdl-main-thread))

(defun init (&rest sdl-init-flags)
  "Initialize SDL3 with the specified subsystems. Initializes everything by default."
  ;; Disable floating point traps for ecl.
  #+ecl(ext:trap-fpe 'division-by-zero nil)

  (unless *wakeup-event*
    (setf *wakeup-event* (alloc 'sdl3-ffi:sdl-event)))
  (unless *main-thread-channel*
    (ensure-main-channel)

    ;; If we did not have a main-thread channel, make a default main thread.
    #-(and (or sbcl ccl) darwin)
    (setf *the-main-thread* (bt:make-thread #'sdl-main-thread :name "SDL3 Main Thread"))

    ;; On OSX, we need to run in the main thread; some implementations allow us to safely
    ;; do this. On other platforms (mainly GLX?), we just need to run in a dedicated thread.
    #+(and ccl darwin)
    (let ((thread (find 0 (ccl:all-processes) :key #'ccl:process-serial-number)))
      (setf *the-main-thread* thread)
      (ccl:process-interrupt thread #'sdl-main-thread)))
    #+(and sbcl darwin)
    (let ((thread (sb-thread:main-thread)))
      (setf *the-main-thread* thread)
      (when (not (eq thread (bt:current-thread)))
        (sb-thread:interrupt-thread thread #'sdl-main-thread)))

  (in-main-thread (:no-event t)
    (let ((init-flags (autowrap:mask-apply 'sdl-init-flags sdl-init-flags)))
      (check-rc (sdl-init init-flags))
      (unless *lisp-message-event*
        (setf *lisp-message-event* (sdl-register-events 1)
              (c-ref *wakeup-event* sdl3-ffi:sdl-event :type) *lisp-message-event*)))))

(defun init* (flags)
  "Low-level function to initialize SDL3 with the supplied subsystems. Useful
   when not using cl-sdl3's threading mechanisms."
  (sdl-init (autowrap:mask-apply 'sdl-init-flags flags)))

(defun was-init (&rest flags)
  (/= 0 (sdl-was-init (autowrap:mask-apply 'sdl-init-flags flags))))

(defun quit ()
  "Shuts down SDL3."
  (in-main-thread (:background t)
    (let ((mtc *main-thread-channel*))
      (sdl-quit)
      (setf *main-thread-channel* nil)
      (setf *lisp-message-event* nil)
      (when mtc (sendmsg mtc nil))))
  #-(and sbcl darwin)
  (when (and *the-main-thread*
             (not (eq *the-main-thread* (bt:current-thread))))
    (handler-case
        (bt:join-thread *the-main-thread*)
      (error (e)
        (declare (ignore e))
        (setf *main-thread-channel* nil)))
    (setf *the-main-thread* nil))
  (when *the-main-thread*
    (setf *the-main-thread* nil))
  ;; Re-enable divsion-by-zero for ecl
  #+ecl(ext:trap-fpe 'division-by-zero nil))

(defun quit* ()
  "Low-level function to quit SDL3. Useful when not using cl-sdl3's
   threading mechanisms."
  (sdl-quit))

(defmacro with-init ((&rest sdl-init-flags) &body body)
  `(progn
     (init ,@sdl-init-flags)
     (unwind-protect
          (in-main-thread () ,@body)
       (quit))))

(defun niy (message)
  (error "SDL3 Error: Construct Not Implemented Yet: ~A" message))

(defun revision ()
  "Get the code revision of SDL that is linked against your program."
  ;; SDL-GET-REVISION returns a strng and a pointer. The pointer should not be
  ;; freed becaues it's a constant in the SDL3 library.
  (nth-value 0 (sdl-get-revision)))

(defun version ()
  "Get the version of SDL that is linked against your program and return the
 major, minor, and micro version."
  (let* ((version (sdl-get-version))
         (major (floor (/ version 1000000)))
         (minor (floor (mod (/ version 1000) 1000)))
         (micro (mod version 1000)))
    (values major minor micro)))

(defun version-wrapped ()
  (values sdl3-ffi:+sdl-major-version+
          sdl3-ffi:+sdl-minor-version+
          sdl3-ffi:+sdl-micro-version+))

(defun get-error ()
  "Get the last error that was set for the current thread."
  (sdl-get-error))
