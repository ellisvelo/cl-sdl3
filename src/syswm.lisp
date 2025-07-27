(in-package :sdl3)

#++
(defun get-window-wm-info (window)
  (c-with ((info sdl3-ffi::sdl-syswm-info))
    (setf (info :version :major) sdl3-ffi:+sdl-major-version+
          (info :version :minor) sdl3-ffi:+sdl-minor-version+
          (info :version :patch) sdl3-ffi:+sdl-patchlevel+)
    (sdl-get-window-wm-info window (info &))
    (let ((subsystem (autowrap:enum-key 'sdl3-ffi:sdl-syswm-type (info :subsystem))))
      (list*
       :subsystem subsystem
       (ecase subsystem
         #+(not (or windows darwin))
         (:x11
          (list :display (info :info :x11 :display)
                :window (info :info :x11 :window)))
         #+windows
         (:windows
          (list :window (info :info :win :window)
                :hdc (info :info :win :hdc))))))))
