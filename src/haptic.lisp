(in-package #:sdl3)

(defun get-haptics ()
  "Get a list of currently connected haptic devices."
  (c-let ((num :int))
    (let ((haptics (sdl-get-haptics (num &))))
      (sdl-array-to-list haptics num))))

(defun is-joystick-haptic-p (joystick)
  (sdl-true-p (sdl-is-joystick-haptic joystick)))

(defun is-mouse-haptic-p ()
  (sdl-true-p (sdl-is-mouse-haptic)))

(defun open-haptic (source)
  "Use this function to open the N'th haptic device for use."
  (check-nullptr (sdl-open-haptic source)))

(defun open-haptic-from-joystick (source)
  "Use this function to open a joystick haptic device for use."
  (check-nullptr (sdl-open-haptic-from-joystick source)))

(defun open-haptic-from-mouse ()
  "Use this function to open the mouses haptic device for use."
  (check-nullptr (sdl-open-haptic-from-mouse)))

(defun close-haptic (haptic)
  "Use this function to close an opened haptic device."
  (sdl-close-haptic haptic))

(defun get-haptic-id (haptic)
  "Get the instance ID of an opened HAPTIC device."
  (sdl-get-haptic-id haptic))

(defun get-haptic-from-id (id)
  "Get the SDL_Haptic associated with an instance ID, if it has been opened."
  (sdl-get-haptic-from-id id))

(defun haptic-rumble-supported-p (haptic)
  "Use this function to test whether rumble is supported on a haptic device."
  (sdl-true-p (sdl-haptic-rumble-supported haptic)))

(defun init-haptic-rumble (haptic)
  "Use this function to initialize a haptic device for simple rumble."
  (sdl-true-p (sdl-init-haptic-rumble haptic)))

(defun play-haptic-rumble (haptic strength length-ms)
  "Play a simple rumble effect on a haptic device."
  (sdl-true-p (sdl-play-haptic-rumble haptic strength length-ms)))

(defun stop-haptic-rumble (haptic)
  "Stop the simple rumble on a haptic device."
  (sdl-true-p (sdl-stop-haptic-rumble haptic)))
