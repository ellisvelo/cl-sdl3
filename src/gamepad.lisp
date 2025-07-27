(in-package #:sdl3)

;; SDL3 has some mappings built-in, but since I have a wonderful Logitech controller that isn't part
;; of the spec yet I embed these here. Also it's a good example of how to provide your own

(defparameter *default-controller-maps* ())

(defmacro add-to-default-controller-maps (&rest maps)
  `(setf *default-controller-maps*
         (append *default-controller-maps* ,@maps)))

(add-to-default-controller-maps
 ;; The primary mode for the F710 is basically an Xbox controller
 '((guid "030000006d0400001fc2000005030000")
   (name "Logitech Wireless Gamepad F710")
   (a b0)
   (b b1)
   (x b2)
   (y b3)
   (start b7)
   (guide b8)
   (back b6)
   (leftshoulder b4)
   (rightshoulder b5)
   (leftstick b9)
   (rightstick b10)
   (dpup h0.1)
   (dpdown h0.4)
   (dpleft h0.8)
   (dpright h0.2)
   (leftx a0)
   (lefty a1)
   (rightx a3)
   (righty a4)
   (lefttrigger a2)
   (righttrigger a5))
 ;; The F710 has a "legacy" mode for systems that don't support a controller that looks like an XBox
 ;; controller
 '((guid "6d0400000000000019c2000000000000")
   (name "Logitech Wireless Gamepad F710 (alt)")
   (a b0)
   (b b1)
   (x b2)
   (y b3)
   (start b7)
   (guide b8)
   (back b6)
   (leftshoulder b4)
   (rightshoulder b5)
   (leftstick b9)
   (rightstick b10)
   (dpup h0.1)
   (dpdown h0.4)
   (dpleft h0.8)
   (dpright h0.2)
   (leftx a0)
   (lefty a1)
   (rightx a4)
   (righty a3)
   (lefttrigger a2)
   (righttrigger a5)))

(defun get-gamepads ()
  "Get a list of currently connected gamepads."
  (c-let ((num :int))
    (let ((gamepads (sdl-get-gamepads (num &))))
      (sdl-array-to-list gamepads num))))

(defun gamepad-p (instance-id)
  "Returns t if the device-index provided belongs to a joystick with a known
gamecontroller mapping."
  (sdl-true-p (sdl-is-gamepad instance-id)))

(defun get-gamepad-name-for-id (instance-id)
  "Return the human readable name for the INSTANCE-ID provided."
  (sdl-get-gamepad-name-for-id instance-id))

(defun open-gamepad (instance-id)
  (check-nullptr (sdl-open-gamepad instance-id)))

(defun close-gamepad (gamepad)
  (sdl-close-gamepad gamepad))

(defun gamepad-connected-p (gamepad)
  (sdl-true-p (sdl-gamepad-connected gamepad)))

(defun add-gamepad-mapping (mapping-string)
  "Use this function to add support for controllers that SDL is unaware of or
to cause an existing controller to have a different binding."
  (sdl-add-gamepad-mapping mapping-string))

(defun get-gamepad-joystick (gamepad)
  (sdl-get-gamepad-joystick gamepad))

(defun get-gamepad-joystick-id (gamepad)
  (get-joystick-id (get-gamepad-joystick gamepad)))

(defun get-gamepad-from-id (joystick-id)
  "Get the SDL_Gamepad associated with a joystick instance ID, if it has been
  opened."
  (sdl-get-gamepad-from-id joystick-id))

(defun add-gamepad-mappings-from-file (filename)
  (sdl-add-gamepad-mappings-from-file filename))

(defun get-gamepad-name (gamepad)
  "Get the implementation-dependent name for an opened gamepad."
  (sdl-get-gamepad-name gamepad))

(defun get-gamepad-axis (gamepad axis)
  "Returns the current value of the GAMEPADS's AXIS based upon the
controller bindings."
  (sdl-get-gamepad-axis gamepad axis))

(defun get-gamepad-axis-from-string (axis-name)
  "Return the SDL_GamepadAxis enum value for AXIS-NAME."
  (check-rc (sdl-get-gamepad-axis-from-string axis-name)))

(defun get-gamepad-mapping (gamepad)
  "Return the mapping for the GAMEPAD."
  (multiple-value-bind (mapping ptr)
      (sdl-get-gamepad-mapping gamepad)
    (sdl-free (autowrap:make-wrapper :ptr ptr))
    mapping))

(defun get-gamepad-button (gamepad button)
  "Return the current state of a button on a gamepad."
  (sdl-get-gamepad-button gamepad button))

(defun get-gamepad-button-from-string (button-name)
  "Return the SDL_GamepadButton enum value for BUTTON-NAME."
  (check-rc (sdl-get-gamepad-button-from-string button-name)))

(defun get-gamepad-string-for-button (button)
  "Return the name for the BUTTON."
  (check-nullptr (sdl-get-gamepad-string-for-button button)))

(defun get-gamepad-button-label (gamepad button)
  "Get the label of a button on a gamepad."
  (sdl-get-gamepad-button-label gamepad button))

(defun get-gamepad-properties (gamepad)
  "Get the properties associated with an opened gamepad."
  (check-zero (sdl-get-gamepad-properties gamepad)))

(defun gamepad-has-rumble-p (gamepad)
  "Return whether a GAMEPAD has rumble support."
  (let ((props (get-gamepad-properties gamepad)))
    (get-boolean-property props +prop-gamepad-cap-rumble-boolean+)))

(defun rumble-gamepad (gamepad low-freq-rumble high-freq-rumble duration-ms)
  "Start a rumble effect on a gamepad."
  (sdl-true-p (sdl-rumble-gamepad gamepad low-freq-rumble high-freq-rumble duration-ms)))
