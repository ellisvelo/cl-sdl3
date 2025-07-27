(in-package :sdl3)

;; A start to the SDL3 audio interface.

;; These are #defines, but really should have been enums....

(autowrap:define-enum-from-constants (sdl-audio-mask)
  sdl3-ffi:+sdl-audio-mask-bitsize+
  sdl3-ffi:+sdl-audio-mask-float+
  sdl3-ffi:+sdl-audio-mask-big-endian+
  sdl3-ffi:+sdl-audio-mask-signed+)

(defun audio-bitsize (x)
  (logand x (enum-value 'sdl-audio-mask :bitsize)))

(defun audio-float-p (x)
  (logand x (enum-value 'sdl-audio-mask :datatype)))

(defun audio-big-endian-p (x)
  (logand x (enum-value 'sdl-audio-mask :endian)))

(defun audio-signed-p (x)
  (logand x (enum-value 'sdl-audio-mask :signed)))

(defun audio-int-p (x)
  (not (audio-float-p x)))

(defun audio-little-endian-p (x)
  (not (audio-big-endian-p x)))

(defun audio-unsigned-p (x)
  (not (audio-signed-p x)))

(defun get-audio-playback-devices ()
  "Get a list of currently-connected audio playback devices."
  (c-let ((num :int))
    (let ((playback-devices (sdl-get-audio-playback-devices (num &))))
      (sdl-array-to-list playback-devices num))))

(defun get-audio-recording-devices ()
  "Get a list of currently-connected audio recording devices."
  (c-let ((num :int))
    (let ((recording-devices (sdl-get-audio-recording-devices (num &))))
      (sdl-array-to-list recording-devices num))))

(defun get-audio-device-name (device-id)
  "Get the human-readable name of a specific audio device."
  (sdl-get-audio-device-name device-id))
