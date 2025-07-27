(in-package :sdl3)

(defun get-num-camera-drivers ()
  "Get the number of built-in camera drivers."
  (sdl-get-num-camera-drivers))

(defun get-camera-driver (index)
  "get the name of a built in camera driver."
  (check-nil (sdl-get-camera-driver index)))

(defun get-current-camera-driver ()
  "Get the name of the current camera driver."
  (check-nil (sdl-get-current-camera-driver)))

(defun get-cameras ()
  "Get a list of currently connected camera devices."
  (c-let ((num :int))
    (let ((cameras (sdl-get-cameras (num &))))
      (sdl-array-to-list cameras num))))

(defun get-camera-supported-formats (camera-id)
  "Get the list of native formats/sizes a camera supports."
  (c-let ((num :int))
    (let ((camera-supported-formats (check-nil (sdl-get-camera-supported-formats camera-id (num &)))))
      (values camera-supported-formats num))))

(defun get-camera-name (camera-id)
  "Get the human-readable device name for a camera."
  (check-nil (sdl-get-camera-name camera-id)))

(defun get-camera-id (camera)
  "Get the instance ID of an opened camera."
  (check-zero (sdl-get-camera-id camera)))

(defun get-camera-format (camera)
  "Get the spec that a camera is using when generating images."
  (c-let ((spec sdl3-ffi:sdl-camera-spec))
    (check-false (sdl-get-camera-format camera (spec &)))
    spec))

(defun get-camera-properties (camera)
  "Get the properties associated with an opened camera."
  (check-zero (sdl-get-camera-properties camera)))

(defun get-camera-permission-state (camera)
  "Query if camera access has been approved by the user."
  (sdl-get-camera-permission-state camera))

(defun open-camera (camera-id &optional camera-spec)
  "Open a video recording device (a \"camera\")."
  (check-nil (sdl-open-camera camera-id camera-spec)))

(defun acquire-camera-frame (camera)
  "Acquire a surface frame."
  (c-let ((timestamp-ns sdl3-ffi:uint64))
    (let ((camera (sdl-acquire-camera-frame camera (timestamp-ns &))))
      (values camera timestamp-ns))))

(defun release-camera-frame (camera surface-frame)
  "Release a frame of video acquired from a camera."
  (sdl-release-camera-frame camera surface-frame))

(defun close-camera (camera)
  "Shut down camera processing and close the camera device"
  (sdl-close-camera camera))
