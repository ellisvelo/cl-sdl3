(in-package :sdl3-examples)

(defun gamepad-test ()
  "Test various gamepad related functions."
  (sdl3:with-init (:video :gamepad)
    (sdl3:with-window (window :title "SDL3 Gamepad Demo")
      (sdl3:with-renderer (renderer window)
        ;; Set the vsync
        (sdl3:set-render-vsync renderer t)

        (let ((gamepads (sdl3:get-gamepads))
              (gamepad-state "No Gamepads detected")
              (open-gamepads '())
              (square (make-instance 'square :x 400.0 :y 200.0)))

          (multiple-value-bind (x y) (sdl3:get-window-size window)
            (with-slots (max-x max-y) square
              (setf max-x (float (- x (square-width square)))
                    max-y (float (- y (square-height square))))))

          (format t "Gamepads count: ~a~%" (list-length gamepads))
          (loop :for gamepad-id :in gamepads
                do (progn
                     (sdl3:open-gamepad gamepad-id)
                     (pushnew gamepad-id open-gamepads)
                     (setf gamepad-state (format nil "Gamepad ~a opened" gamepad-id))))

          (format t "Gamepads: ~a~%" open-gamepads)

          (sdl3:with-event-loop (:method :poll)
            (:key-down (:scancode scancode)
                       (when (sdl3:scancode= scancode :escape)
                         (sdl3:push-event :quit)))

            (:gamepad-button-down
             (:button button-id)
             (when (= button-id sdl3:+gamepad-button-guide+)
               (sdl3:push-event :quit)))

            (:gamepad-axis-motion
             (:which gamepad-id :axis axis-id :value value)
             (let ((text (format nil "Gamepad axis motion: Gamepad: ~a, Axis: ~a, Value: ~a~%" gamepad-id axis-id value)))
               (setf gamepad-state text)
               (handle-gamepad-motion square axis-id value)))

            (:gamepad-added
             (:which gamepad-id)
             (sdl3:open-gamepad gamepad-id)
             (pushnew gamepad-id open-gamepads)
             (setf gamepad-state (format nil "Gamepad ~a opened: ~a" gamepad-id (sdl3:get-gamepad-name-for-id gamepad-id))))

            (:gamepad-removed
             (:which gamepad-id)
             (sdl3:close-gamepad (sdl3:get-gamepad-from-id gamepad-id))
             (setf open-gamepads (remove gamepad-id open-gamepads))
             (setf gamepad-state (format nil "Gamepad ~a closed" gamepad-id)))

            (:idle ()
                   (sdl3:set-render-draw-color renderer 0 0 0 255)
                   (sdl3:render-clear renderer)
                   (sdl3:set-render-draw-color renderer 255 255 255 255)
                   (sdl3:render-debug-text renderer 175.0 100.0 gamepad-state)

                   (move-square square)
                   (render-square renderer square)

                   (sdl3:render-present renderer))

            (:quit ()
                   (loop :for gamepad-id :in open-gamepads
                         do (sdl3:close-gamepad (sdl3:get-gamepad-from-id gamepad-id)))
                   t)))))))
