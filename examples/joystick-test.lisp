(in-package :sdl3-examples)

(defun joystick-test ()
  "Test various joystick related functions."
  (sdl3:with-init (:video :joystick)
    (sdl3:with-window (window :title "SDL3 Joystick Demo")
      (sdl3:with-renderer (renderer window)
	;; Set the vsync
	(sdl3:set-render-vsync renderer t)

	(let* ((joysticks (sdl3:get-joysticks))
	       (open-joysticks '())
	       (joystick-state "No Joysticks detected")
	       (square (make-instance 'square :x 400.0 :y 200.0)))

	  (multiple-value-bind (x y) (sdl3:get-window-size window)
	    (with-slots (max-x max-y) square
	      (setf max-x (float (- x (square-width square)))
		    max-y (float (- y (square-height square))))))

	  (format t "Joysticks count: ~a~%" (list-length joysticks))
	  (loop :for joystick-id :in joysticks
		do (progn
		     (sdl3:open-joystick joystick-id)
		     (pushnew joystick-id open-joysticks)
		     (setf joystick-state (format nil "Joystick ~a opened" joystick-id))))

          (sdl3:with-event-loop (:method :poll)
            (:key-down (:scancode scancode)
                       (when (sdl3:scancode= scancode :escape)
			 (sdl3:push-event :quit)))

	    (:joystick-button-down
	     (:button button-id)
	     (when (= button-id sdl3:+gamepad-button-guide+)
	       (sdl3:push-event :quit)))

	    (:joystick-axis-motion
	     (:which joystick-id :axis axis-id :value value)
             (let ((text (format nil "Joystick axis motion: Joystick: ~a, Axis: ~a, Value: ~a~%" joystick-id axis-id value)))
	       (setf joystick-state text)
	       (handle-gamepad-motion square axis-id value)))

	    (:joystick-added
	     (:which joystick-id)
	     (sdl3:open-joystick joystick-id)
	     (pushnew joystick-id open-joysticks)
	     (setf joystick-state (format nil "Joystick ~a opened: ~a" joystick-id (sdl3:get-joystick-name-for-id joystick-id))))

	    (:joystick-removed
	     (:which joystick-id)
	     (sdl3:close-joystick (sdl3:get-joystick-from-id joystick-id))
	     (setf open-joysticks (remove joystick-id open-joysticks))
	     (setf joystick-state (format nil "Joystick ~a closed" joystick-id)))

            (:idle ()
		   (sdl3:set-render-draw-color renderer 0 0 0 255)
		   (sdl3:render-clear renderer)
		   (sdl3:set-render-draw-color renderer 255 255 255 255)
		   (sdl3:render-debug-text renderer 175.0 100.0 joystick-state)

		   (move-square square)
		   (render-square renderer square)

		   (sdl3:render-present renderer))

            (:quit ()
		   (loop :for joystick-id :in open-joysticks
			 do (sdl3:close-joystick (sdl3:get-joystick-from-id joystick-id)))
		   t)))))))
