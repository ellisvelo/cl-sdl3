(in-package :sdl3-examples)

(require :sdl3)
(require :cl-opengl)

(defun basic-test ()
  "The kitchen sink."
  (sdl3:with-init (:everything)
    (multiple-value-bind (major minor micro) (sdl3:version)
      (format t "Using SDL Library Version: ~D.~D.~D~%" major minor micro))

    (finish-output)

    (sdl3:with-window (win :flags '(:opengl))
      (sdl3:with-gl-context (gl-context win)
        (let ((gamepads ())
              (haptic ()))

          ;; basic window/gl setup
          (format t "Setting up window/gl.~%")
          (finish-output)
          (sdl3:gl-make-current win gl-context)
          (gl:viewport 0 0 800 600)
          (gl:matrix-mode :projection)
          (gl:ortho -2 2 -2 2 -2 2)
          (gl:matrix-mode :modelview)
          (gl:load-identity)
          (gl:clear-color 0.0 0.0 1.0 1.0)
          (gl:clear :color-buffer)

          (format t "Opening gamepads.~%")
          (finish-output)
          ;; open any game gamepads
	  (let ((joysticks (sdl3:get-joysticks)))
	    (format t "Joystick count: ~a~%" (list-length joysticks))
	    (mapc #'(lambda (id)
		      (format t "Found gamepad: ~a~%" (sdl3:get-gamepad-name-for-id id))
		      (let* ((gp (sdl3:open-gamepad id))
			     (joy (sdl3:get-gamepad-joystick gp)))
			(when (sdl3:joystick-has-rumble-p joy)
			  (format t "Rumble supported ~a~%" joy)
			  (sdl3:rumble-joystick joy #x3FFF  #xFFFF 500))

			(when (sdl3:is-joystick-haptic-p joy)
			  (format t "Found haptic for ~a~%" joy)
                              (let ((h (sdl3:open-haptic-from-joystick joy)))
				(setf haptic (acons id h haptic))
				(sdl3:init-haptic-rumble h)))

			(setf gamepads (acons id gp gamepads)))) joysticks))

	  ;; Print any open audio playback devices
	  (let ((playback-devices (sdl3:get-audio-playback-devices)))
	    (mapc #'(lambda (id)
		      (format t "Audio Playback: ~a~%" (sdl3:get-audio-device-name id))) playback-devices))

          ;; main loop
          (format t "Beginning main loop.~%")
          (finish-output)
          (sdl3:with-event-loop (:method :poll)
            (:key-down (:scancode scancode)
                       (cond
			 ((sdl3:scancode= scancode :e) (error "Demonstrate SDL restarts"))
                         ((sdl3:scancode= scancode :w)
			  (format t "~a~%" "WALK"))

                         ((sdl3:scancode= scancode :s)
			  (sdl3:show-cursor))

                         ((sdl3:scancode= scancode :h)
			  (sdl3:hide-cursor))))

            (:key-up (:scancode scancode)
                     (when (sdl3:scancode= scancode :escape)
                      (sdl3:push-event :quit)))

            (:mouse-motion (:x x :y y :xrel xrel :yrel yrel :state state)
                           (format t "Mouse motion abs(rel): ~a (~a), ~a (~a)~%Mouse state: ~a~%"
                                   x xrel y yrel state))

            (:gamepad-axis-motion
	     (:which controller-id :axis axis-id :value value)
             (format t "Controller axis motion: Controller: ~a, Axis: ~a, Value: ~a~%"
                     controller-id axis-id value))

            (:gamepad-button-down (:which controller-id)
				  (let ((h (cdr (assoc controller-id haptic))))
				    (when h
				      (sdl3:rumble-gamepad h 0.1 1.0 100))))

            (:idle ()
                   (gl:clear :color-buffer)
                   (gl:begin :triangles)
                   (gl:color 1.0 0.0 0.0)
                   (gl:vertex 0.0 1.0)
                   (gl:vertex -1.0 -1.0)
                   (gl:vertex 1.0 -1.0)
                   (gl:end)
                   (gl:flush)
                   (sdl3:gl-swap-window win))

            (:quit () t))

          (format t "Closing opened gamepads.~%")
          (finish-output)
          ;; close any game gamepads that were opened as well as any haptics
          (loop :for (i . gamepad) :in gamepads
                :do (sdl3:close-gamepad gamepad)
                    (sdl3:close-haptic (cdr (assoc i haptic)))))))))
