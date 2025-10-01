(in-package #:sdl3)

(defmacro update-joysticks ()
  "Use this function to update the current state of the open joysticks.
This function is called automatically by the event loop if joystick events are
 enabled."
  `(sdl-update-joysticks))

(defun get-joysticks ()
  "Get a list of currently connected joysticks."
  (c-let ((num :int))
    (let ((joysticks (sdl-get-joysticks (num &))))
      (sdl-array-to-list joysticks num))))

(defun open-joystick (device-index)
  (check-nil (sdl-open-joystick device-index)))

(defun close-joystick (joystick)
  (sdl-close-joystick joystick))

(defun get-joystick-name (joystick)
  (check-nil (sdl-get-joystick-name joystick)))

(defun get-joystick-name-for-id (instance-id)
  "Return the human readable name for the INSTANCE-ID provided."
  (sdl-get-joystick-name-for-id instance-id))

(defun get-joystick-from-id (instance-id)
  "Get the SDL_Joystick associated with an instance ID, if it has been opened."
  (sdl-get-joystick-from-id instance-id))

(defun get-num-joystick-hats (joystick)
  (check-rc (sdl-get-num-joystick-hats joystick)))

(defun get-num-joystick-axes (joystick)
  (check-rc (sdl-get-num-joystick-axes joystick)))

(defun get-num-joystick-balls (joystick)
  (check-rc (sdl-get-num-joystick-balls joystick)))

(defun get-num-joystick-buttons (joystick)
  (check-rc (sdl-get-num-joystick-buttons joystick)))

(defun get-joystick-id (joystick)
  (check-zero (sdl-get-joystick-id joystick)))

(defun get-joystick-axis (joystick axis)
  (check-zero (sdl-get-joystick-axis joystick axis)))

(defun get-joystick-hat (joystick hat)
  "Return the current state of a hat"
  (sdl-get-joystick-hat joystick hat))

(defun get-joystick-properties (joystick)
  "Get the properties associated with a joystick."
  (check-zero (sdl-get-joystick-properties joystick)))

(defun get-joystick-connection-state (joystick)
  "Get the connection state of a joystick."
  (let ((state (sdl-get-joystick-connection-state joystick)))
    (if (= state sdl3-ffi:+sdl-joystick-connection-unknown+)
        (error 'sdl-error :string (sdl-get-error))
        (autowrap:enum-key 'sdl3-ffi:sdl-joystick-connection-state state))))

(defun rumble-joystick (joystick low-frequency-rumble high-frequency-rumble duration-ms)
  "Start a rumble effect for a duration of DURATION-MS. The low and high
frequences are between 0 and 0xFFFF."
  (sdl-true-p (sdl-rumble-joystick joystick low-frequency-rumble high-frequency-rumble duration-ms)))

(defun joystick-has-rumble-p (joystick)
  "Return T when the JOYSTICK has rumble effects."
  (get-boolean-property (get-joystick-properties joystick) +prop-joystick-cap-rumble-boolean+))
