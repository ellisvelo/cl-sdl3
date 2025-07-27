(in-package #:sdl3)

(defun get-num-video-drivers ()
  (sdl-get-num-video-drivers))

(defun get-video-driver (driver-index)
  (sdl-get-video-driver driver-index))

(defun get-current-video-driver ()
  (sdl-get-current-video-driver))

(defun get-num-video-displays ()
  (with-foreign-objects ((count :int))
    (sdl-get-displays count)
    (mem-ref count :int)))

(defun get-display-name (display-index)
  (sdl-get-display-name display-index))

(defun get-fullscreen-display-modes (displayId)
  ;; Needs more work
  (with-foreign-objects ((num :int))
    (values (sdl-get-fullscreen-display-modes displayId (mem-ref num :int)) num)))

(defun get-primary-display ()
  "Return the primary display."
  (check-zero (sdl-get-primary-display)))

(defun get-current-display-mode (display-id)
  "Get information about the current display mode."
  (let ((result (check-nullptr (sdl-get-current-display-mode display-id))))
    (c-let ((display-mode sdl3-ffi:sdl-display-mode :from result))
      (values (display-mode :format)
	      (display-mode :w)
	      (display-mode :h)
	      (display-mode :refresh-rate)))))

(defun get-display-bounds (display-index)
  "Use this function to get the desktop area represented by a display, with the primary display
located at 0,0."
  (let-rects (rect)
    (check-rc (sdl3-ffi.functions:sdl-get-display-bounds display-index (rect &)))
    rect))

(autowrap:define-bitmask-from-constants (sdl-window-flags)
  sdl3-ffi:+sdl-window-fullscreen+
  sdl3-ffi:+sdl-window-opengl+
  sdl3-ffi:+sdl-window-occluded+
  sdl3-ffi:+sdl-window-hidden+
  sdl3-ffi:+sdl-window-borderless+
  sdl3-ffi:+sdl-window-resizable+
  sdl3-ffi:+sdl-window-minimized+
  sdl3-ffi:+sdl-window-maximized+
  sdl3-ffi:+sdl-window-mouse-grabbed+
  sdl3-ffi:+sdl-window-input-focus+
  sdl3-ffi:+sdl-window-mouse-focus+
  sdl3-ffi:+sdl-window-external+
  sdl3-ffi:+sdl-window-modal+
  sdl3-ffi:+sdl-window-high-pixel-density+
  sdl3-ffi:+sdl-window-mouse-capture+
  sdl3-ffi:+sdl-window-mouse-relative-mode+
  sdl3-ffi:+sdl-window-always-on-top+
  sdl3-ffi:+sdl-window-utility+
  sdl3-ffi:+sdl-window-tooltip+
  sdl3-ffi:+sdl-window-popup-menu+
  sdl3-ffi:+sdl-window-keyboard-grabbed+
  sdl3-ffi:+sdl-window-vulkan+
  sdl3-ffi:+sdl-window-metal+
  sdl3-ffi:+sdl-window-transparent+
  sdl3-ffi:+sdl-window-not-focusable+)

(autowrap:define-bitmask-from-enum
    (sdl-gl-attribute sdl3-ffi:sdl-gl-attr))

(defun windowpos-undefined (&optional (display 0))
  (logior sdl3-ffi:+sdl-windowpos-undefined-mask+ display))

(defun windowpos-centered (&optional (display 0))
  (logior sdl3-ffi:+sdl-windowpos-centered-mask+ display))

(defun windowpos-from-coord (n)
  (case n
    (:undefined (windowpos-undefined))
    (:centered (windowpos-centered))
    (t n)))

(defun create-window (&key (title "SDL3 Window") (x :centered) (y :centered) (w 800) (h 600) (flags 0))
  (let* ((window-flags (mask-apply 'sdl-window-flags flags))
	 (window (check-nullptr (sdl-create-window title w h window-flags))))
    (set-window-position window x y)
    window))

(defun destroy-window (win)
  (sdl-destroy-window win)
  (autowrap:invalidate win)
  (values))

(defmacro with-window ((win &key (title "SDL3 Window") (x :centered) (y :centered) (w 800) (h 600) flags)
		       &body body)
  "Creates an SDL3 window, executes the body of code, and then destroys the
window."
  `(let ((,win (create-window :title ,title :x ,x :y ,y :w ,w :h ,h :flags ,flags)))
     (unwind-protect (progn ,@body)
       (destroy-window ,win))))

(defmacro with-everything ((&key window gl) &body body)
  (assert (and window gl))
  (let ((window (if (symbolp window) (list window) window)))
    (destructuring-bind (win &key (title "SDL3 Window") (w 800) (h 600)
                               (flags ''(:shown :opengl)) (fullscreen nil))
        window
      `(with-init (:everything)
         (with-window (,win :title ,title :w ,w :h ,h :flags ,flags)
           (with-gl-context (,gl ,win)
             (set-window-fullscreen ,win ,fullscreen)
             ,@body))))))

(defun hide-window (win)
  (sdl-hide-window win))

(defun show-window (win)
  (sdl-show-window win))

(defun maximize-window (win)
  (sdl-maximize-window win))

(defun minimize-window (win)
  (sdl-minimize-window win))

(defun raise-window (win)
  (sdl-raise-window win))

(defun restore-window (win)
  (sdl-restore-window win))

(defun update-window (win)
  (check-rc (sdl-update-window-surface win)))

(defun set-window-title (win title)
  (sdl-set-window-title win title))

(autowrap:define-enum-from-constants (sdl-window-fullscreen "SDL-WINDOW-")
  sdl3-ffi:+sdl-window-fullscreen+)

(defun set-window-fullscreen (win fullscreen-value)
  "`FULLSCREEN-VALUE` of `t` or `:fullscreen` is \"regular\" fullscreen,
`SDL_WINDOW_FULLSCREEN`. Specifying `:windowed` or `:desktop` is \"windowed\"
fullscreen, using `SDL_WINDOW_FULLSCREEN_DESKTOP`."
  (let ((flag (case fullscreen-value
                (nil 0)
                ((:desktop :windowed) :fullscreen-desktop)
                ((t :fullscreen) :fullscreen))))
    (check-rc (sdl-set-window-fullscreen
               win (if flag (enum-value 'sdl-window-fullscreen flag) 0)))))

(defun set-window-size (win w h)
  (sdl-set-window-size win w h))

(defun set-window-position (win x y)
  "Request that the window's position be set."
  (check-false (sdl-set-window-position win
					(windowpos-from-coord x)
					(windowpos-from-coord y))))

(defun get-window-title (win)
  (sdl-get-window-title win))

(defun get-window-position (win)
  (with-foreign-objects ((xpos :int)
                         (ypos :int))
    (sdl-get-window-position win xpos ypos)
    (values (mem-ref xpos :int) (mem-ref ypos :int))))

(defun get-window-size (win)
  (with-foreign-objects ((width :int)
                         (height :int))
    (sdl-get-window-size win width height)
    (values (mem-ref width :int) (mem-ref height :int))))

(defun get-window-aspect-ratio (win)
  (multiple-value-call #'/ (get-window-size win)))

(defun get-window-surface (win)
  ;; Do NOT free the returned surface.
  (check-nullptr (sdl-get-window-surface win)))

(defun get-window-flags (win)
  (let ((flags (sdl-get-window-flags win)))
    (autowrap:mask-keywords 'sdl-window-flags flags)))

(defun get-window-pixel-format (win)
  "Use this function to get the pixel format associated with the window."
  (enum-key 'sdl3-ffi:sdl-pixel-format (sdl-get-window-pixel-format win)))

(declaim (inline get-window-id))
(defun get-window-id (win)
  (sdl-get-window-id win))

(defun get-window-display-index (window)
  (sdl3-ffi.functions:sdl-get-display-for-window window))

(defun enable-screensaver ()
  (sdl-enable-screen-saver))

(defun disable-screensaver ()
  (sdl-disable-screen-saver))

(defun screensaver-enabled-p ()
  (sdl-screen-saver-enabled))

(defun gl-create-context (win)
  (check-nullptr (sdl-gl-create-context win)))

(defun gl-delete-context (gl-context)
  (sdl-gl-destroy-context gl-context)
  (autowrap:invalidate gl-context)
  (values))

(defmacro with-gl-context ((gl-context-sym win) &body body)
  `(let ((,gl-context-sym (sdl3:gl-create-context ,win)))
     (unwind-protect
          (progn ,@body)
       (sdl3:gl-delete-context ,gl-context-sym))))

(defun gl-extension-supported-p (extension)
  (sdl-gl-extension-supported extension))

(defun gl-make-current (win gl-context)
  (check-rc (sdl-gl-make-current win gl-context)))

(defun gl-get-swap-interval (interval)
  (sdl-gl-get-swap-interval interval))

(defun gl-set-swap-interval (interval)
  "0 for immediate updates, 1 for updates synchronized with the vertical retrace"
  (check-rc (sdl-gl-set-swap-interval interval)))

(defun gl-swap-window (win)
  (sdl-gl-swap-window win))

(defun gl-get-attr (attr)
  (with-foreign-object (value :int)
    (check-rc (sdl-gl-get-attribute (autowrap:mask 'sdl-gl-attr attr) value))
    (mem-ref value :int)))

(defun gl-get-attrs (&rest attrs)
  (mapcan #'list attrs (mapcar #'gl-get-attr attrs)))

(defun gl-set-attr (attr value)
  (check-rc (sdl-gl-set-attribute (autowrap:mask 'sdl-gl-attr attr) value)))

(defun gl-set-attrs (&rest attr-plist)
  (loop :for (attr value) :on attr-plist :by #'cddr
        :do (gl-set-attr attr value)))

(defun gl-get-proc-address (proc-name)
  (sdl-gl-get-proc-address proc-name))
