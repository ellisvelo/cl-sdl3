(in-package #:sdl3)

(defmacro warp-mouse-in-window (win x y)
  "Use this function to move the mouse to the given position within the window."
  `(sdl-warp-mouse-in-window ,win ,x ,y))

(defun hide-cursor ()
  "Set the cursor to invisible in all SDL2 windows."
  (sdl-true-p (check-rc (sdl-hide-cursor))))

(defun show-cursor ()
  "Set the cursor to visible in all SDL2 windows."
  (sdl-true-p (check-rc (sdl-show-cursor))))

(defun cursor-visible-p ()
  "Returns whether the cursor is currently visible."
  (sdl-true-p (sdl-cursor-visible)))

(defun set-relative-mouse-mode (window enabled)
  (let ((enabled (ecase enabled ((1 t) 1) ((0 nil) 0))))
    (check-rc (sdl-set-window-relative-mouse-mode window enabled))))

(defun relative-mouse-mode-p (window)
  (sdl-true-p (sdl-get-window-relative-mouse-mode window)))

(defun toggle-relative-mouse-mode (window)
  (set-relative-mouse-mode window (relative-mouse-mode-p window)))

(defun mouse-state ()
  "Returns (VALUES X Y BITMASK) where X, Y give the mouse cursor position relative to the focused
window and BITMASK has bit i from the right set if and only if mouse button i is pressed."
  (c-with ((x :int) (y :int))
    (let ((buttons (sdl3-ffi.functions:sdl-get-mouse-state (x &) (y &))))
      (values x y buttons))))

(defun mouse-state-p (button)
  "Whether the mouse button numbered BUTTON is pressed inside the focused window. 1 indicates the
left mouse button, 2 the middle mouse button and 3 the right mouse button."
  (let ((buttons (sdl3-ffi.functions:sdl-get-mouse-state nil nil))
        (mask (ash 1 (1- button))))
    (plusp (logand buttons mask))))

(defun get-global-mouse-state ()
  "Returns (X Y BITMASK) where X and Y are positions of the mouse cursor relative to the desktop
and BITMASK has bit i from the right set if and only if mouse button i is pressed."
  (c-with ((x :int) (y :int))
    (let ((buttons (sdl3-ffi.functions:sdl-get-global-mouse-state (x &) (y &))))
      (values x y buttons))))

(defun global-mouse-state-p (button)
  "Whether the mouse button numbered BUTTON is pressed. 1 indicates the left mouse button,
2 the middle mouse button and 3 the right mouse button. This function works relative to desktop
and can be used even when there is no SDL window open."
  (let ((buttons (sdl3-ffi.functions:sdl-get-global-mouse-state nil nil))
        (mask (ash 1 (1- button))))
    (plusp (logand buttons mask))))
