(in-package #:sdl3)

;;(autowrap:define-bitmask-from-enum (keymod sdl3-ffi::sdl-keymod))

(defun key-down-p (state)
  (= state sdl3-ffi:+sdl-keydown+))

(defun key-up-p (state)
  (= state sdl3-ffi:+sdl-keyup+))

(defun scancode-value (key &optional modstate)
  "Converts a key to the numerical value of its scancode."
  (sdl3-ffi.functions:sdl-get-scancode-from-key key modstate))

(defun scancode (key)
  "Converts a key to a scancode keyword."
  (autowrap:enum-key 'sdl3-ffi:sdl-scancode (scancode-value key)))

(defun scancode-symbol (scancode)
  "Converts a scancode number to a scancode keyword."
  (autowrap:enum-key 'sdl3-ffi:sdl-scancode scancode))

(defun scancode-key-to-value (scancode-key)
  "Converts a scancode keyword to its numerical value."
  (autowrap:enum-value 'sdl3-ffi:sdl-scancode scancode-key))

(defun scancode= (scancode scancode-key)
  (= scancode (scancode-key-to-value scancode-key)))

(defun mod-keywords (value)
  (autowrap:mask-keywords 'keymod value))

(defun mod-value-p (value &rest keywords)
  (let ((mask (autowrap:mask-apply 'keymod keywords)))
    (/= 0 (logand mask value))))

(defun get-mod-state ()
  (sdl3-ffi.functions:sdl-get-mod-state))

(defun mod-key-state-p (key)
  (mod-value-p (get-mod-state) key))

(defun keyboard-state-p (scancode)
  "Whether the key corresponding to the given scancode is currently pressed."
  (let ((state (nth-value 1 (autowrap:inhibit-string-conversion
                              (sdl3-ffi.functions:sdl-get-keyboard-state nil))))
        (scancode-num (autowrap:enum-value 'sdl3-ffi:sdl-scancode scancode)))
    (c-let ((state :unsigned-char :ptr state))
      (= (state scancode-num) 1))))

(defun get-key-from-scancode (scancode modstate key-event)
  (sdl-get-key-from-scancode scancode modstate key-event))

(defun get-key-name (key)
  (values (sdl-get-key-name key)))

(defun scancode-name (scancode modstate key-event)
  (get-key-name (get-key-from-scancode scancode modstate key-event)))

(defun scancode-key-name (scancode modstate key-event)
  (let ((key (sdl-get-key-from-scancode scancode modstate key-event)))
    (values (sdl-get-key-name key))))

(defun start-text-input (window)
  (sdl3-ffi.functions:sdl-start-text-input window))

(defun stop-text-input (window)
  (sdl3-ffi.functions:sdl-stop-text-input window))
