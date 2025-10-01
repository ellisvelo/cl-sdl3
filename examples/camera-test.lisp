(in-package :sdl3-examples)

(defun add-camera (camera-id open-cameras)
  (if (not (assoc camera-id open-cameras))
      (progn
        (format t "Opened Camera: ~a ~a~%" camera-id (sdl3:get-camera-name camera-id))
        (acons camera-id (sdl3:open-camera camera-id) open-cameras))
      open-cameras))

(defun remove-camera (camera-id open-cameras)
  (sdl3:close-camera (cdr (assoc camera-id open-cameras)))
  (remove-if #'(lambda (x) (eq (car x) camera-id)) open-cameras)
  (format t "Removed Camera: ~a ~a~%" camera-id (sdl3:get-camera-name camera-id)))

(defun print-supported-formats (camera-id)
  (multiple-value-bind (supported-formats num) (sdl3:get-camera-supported-formats camera-id)
    (dotimes (i num)
      (plus-c:c-let ((spec sdl3-ffi:sdl-camera-spec :from (autowrap:c-aref supported-formats i 'sdl3-ffi:sdl-camera-spec)))
        (format t "Format : Width: ~a Height: ~a ~a~%" (spec :width) (spec :height) (sdl3:get-pixel-format-name (spec :format)))))))

(defun create-texture (renderer frame)
  (plus-c:c-let ((surface-frame sdl3-ffi:sdl-surface :from frame))
    (let ((props (sdl3:create-properties))
          (texture))
      (sdl3:set-number-property props sdl3:+prop-texture-create-format-number+ (surface-frame :format))
      (sdl3:set-number-property props sdl3:+prop-texture-create-colorspace-number+ (sdl3:get-surface-colorspace frame))
      (sdl3:set-number-property props sdl3:+prop-texture-create-access-number+ sdl3:+textureaccess-streaming+)
      (sdl3:set-number-property props sdl3:+prop-texture-create-width-number+ (surface-frame :w))
      (sdl3:set-number-property props sdl3:+prop-texture-create-height-number+ (surface-frame :h))
      (setf texture (sdl3:create-texture-with-properties renderer props))
      (sdl3:destroy-properties props)
      texture)))

(defun update-texture (texture frame)
  (plus-c:c-let ((surface-frame sdl3-ffi:sdl-surface :from frame))
    (sdl3:update-texture texture nil (surface-frame :pixels) (surface-frame :pitch))))

(defun process-camera-frame (renderer frame)
  (plus-c:c-let ((surface-frame sdl3-ffi:sdl-surface :from frame))
    (sdl3:create-texture renderer (surface-frame :format) sdl3:+textureaccess-streaming+ (surface-frame :w) (surface-frame :h))))

(defun camera-test ()
  "Test various camera related functions."
  (sdl3:with-init (:video :camera)
    (sdl3:with-window (window :title "SDL3 Camera Demo")
      (sdl3:with-renderer (renderer window)
        ;; Set the vsync
        (sdl3:set-render-vsync renderer t)

        (let ((cameras (sdl3:get-cameras))
              (camera-state "No cameras detected")
              (open-cameras '())
              (camera)
              (texture))

          (format t "Cameras count: ~a~%" (list-length cameras))
          (format t "Cameras current driver: ~a~%" (sdl3:get-current-camera-driver))
          (when (> (list-length cameras) 0)
            ;; Use the first camear
            (let ((camera-id (car cameras)))
              (format t "Camera name: ~a~%" (sdl3:get-camera-name camera-id))
              (setf open-cameras (add-camera camera-id open-cameras))
              (setf camera (cdr (assoc camera-id open-cameras)))
              (setf camera-state (format nil "Camera ~a opened" camera-id))

              ;; print the formats supported
              (print-supported-formats camera-id)))

          (sdl3:with-event-loop (:method :poll)
            (:key-down (:scancode scancode)
                       (when (sdl3:scancode= scancode :escape)
                         (sdl3:push-event :quit)))

            (:camera-device-approved
             (:which camera-id)
             (format t "Camera: ~a approved: ~a~%" camera-id (sdl3:get-camera-name camera-id)))

            (:camera-device-added
             (:which camera-id)
             (setf open-cameras (add-camera camera-id open-cameras))
             (setf camera-state (format nil "Camera ~a opened: ~a" camera-id (sdl3:get-camera-name camera-id))))

            (:camera-device-denied
             (:which camera-id)
             (format t "Camera: ~a denied~%" camera-id))

            (:camera-device-removed
             (:which camera-id)
             (setf open-cameras (remove-camera camera-id open-cameras))
             (setf camera-state (format nil "Camera ~a closed" camera-id)))

            (:idle ()
                   (sdl3:set-render-draw-color renderer 0 0 0 255)
                   (sdl3:render-clear renderer)
                   (sdl3:set-render-draw-color renderer 255 255 255 255)
                   (sdl3:render-debug-text renderer 175.0 100.0 camera-state)

                   (when camera
                     (let ((frame (sdl3:acquire-camera-frame camera)))
                       (when (not (autowrap:wrapper-null-p frame))
                         (if (null texture)
                             (setf texture (create-texture renderer frame))
                             (update-texture texture frame))
                         ;; (process-camera-frame renderer frame)
                         (sdl3:release-camera-frame camera frame))))

                   (sdl3:with-f-rects ((dest-rect 200.0 200.0 300.0 300.0))
                     (sdl3:render-texture renderer texture :dest-f-rect dest-rect))
                   (sdl3:render-present renderer))

            (:quit ()
                   (format t "Closing cameras: ~a~%" open-cameras)
                   (loop :for (i . camera) :in open-cameras
                         do (sdl3:close-camera camera))
                   t)))))))
