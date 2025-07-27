(in-package :sdl3)

;; Create the keywords for the SDL_RendererFlip enum.
(autowrap:define-bitmask-from-enum (sdl-renderer-flip sdl3-ffi:sdl-flip-mode))

(defun get-num-render-drivers ()
  "Return the number of 2D rendering drivers available for the current display."
  (sdl-get-num-render-drivers))

(defun create-window-and-renderer (title width height &optional window-flags)
  (c-let ((winptr :pointer :free t)
          (rendptr :pointer :free t))
    (check-rc (sdl-create-window-and-renderer
	       title
	       width
	       height
	       (if window-flags
		   (mask-apply 'sdl3-ffi:sdl-window-flags window-flags)
		   0)
	       (winptr &)
	       (rendptr &)))
    (let ((window (sdl3-ffi::make-sdl-window :ptr winptr))
          (renderer (sdl3-ffi::make-sdl-renderer :ptr rendptr)))
      (values window renderer))))

(defun create-renderer (window driver-name)
  "Create a 2D rendering context for a window."
  (check-nullptr (sdl-create-renderer window driver-name)))

(defun create-renderer-with-properties (props)
  "Create a 2D rendering context for a window, with the specified properties."
  (check-nullptr (sdl-create-renderer-with-properties props)))

(defun create-software-renderer (surface)
  "Create and return a 2D software rendering context for the surface."
  (check-nullptr (sdl-create-software-renderer surface)))

(defun destroy-renderer (r)
  (sdl-destroy-renderer r)
  (invalidate r))

(defmacro with-renderer ((renderer-sym window &optional driver-name) &body body)
  `(let ((,renderer-sym (sdl3:create-renderer ,window ,driver-name)))
     (unwind-protect
          (progn ,@body)
       (sdl3:destroy-renderer ,renderer-sym))))

(defun get-renderer (window)
  "Return NIL if there is no renderer associated with the window, or otherwise the SDL_Renderer
structure."
  (let ((renderer (sdl-get-renderer window)))
    (if (null-pointer-p (autowrap:ptr renderer))
        nil
        renderer)))

(defun render-texture (renderer texture &key source-f-rect dest-f-rect)
  "Use this function to copy a portion of the texture to the current rendering
target at subpixel precision."
  (check-rc (sdl-render-texture renderer texture source-f-rect dest-f-rect)))

(defun render-texture-rotated (renderer texture &key source-f-rect dest-f-rect angle center flip)
  "Use this function to copy a portion of the texture to the current rendering
target, optionally rotating it by angle around the given center and also
flipping it top-bottom and/or left-right."
  (check-rc (sdl-render-texture-rotated
             renderer
             texture
             source-f-rect
             dest-f-rect
             (coerce (or angle 0) 'double-float)
             center
             (mask-apply 'sdl-renderer-flip flip))))

(defun get-render-vsync (renderer)
  "Get the VSync of the given renderer."
  (c-let ((vsync :int))
    (sdl-true-p (sdl-get-render-v-sync renderer (vsync &)))
    (sdl-true-p vsync)))

(defun set-render-vsync (renderer vsync)
  "Toggle VSync of the given renderer."
  (sdl-true-p (sdl-set-render-v-sync renderer (boolean-to-bool vsync))))

(defun set-render-draw-color (renderer r g b a)
  "Use this function to set the color used for drawing operations (Rect, Line and Clear)."
  (check-rc (sdl-set-render-draw-color renderer r g b a)))

(defun get-render-draw-color (renderer)
  "Use this function to get the current color used by renderer for drawing operations"
  (c-with ((r sdl3-ffi:uint8)
	   (g sdl3-ffi:uint8)
	   (b sdl3-ffi:uint8)
	   (a sdl3-ffi:uint8))
    (check-rc (sdl-get-render-draw-color renderer (r &) (g &) (b &) (a &)))
    (values r g b a)))

(defun set-texture-blend-mode (texture blend-mode)
  "Use this function to set the blend mode for a texture, used by SDL_RenderCopy()."
  (check-rc (sdl-set-texture-blend-mode texture blend-mode)))

(defun get-texture-blend-mode (texture)
  "Use this function to get the blend mode used for texture copy operations."
  (c-with ((blend-mode sdl3-ffi:sdl-blend-mode))
    (check-rc (sdl-get-texture-blend-mode texture (blend-mode &)))
    blend-mode))

(defun set-render-draw-blend-mode (renderer blend-mode)
  "Use this function to set the blend mode used for drawing operations (Fill and Line)."
  (check-rc (sdl-set-render-draw-blend-mode renderer blend-mode)))

(defun set-render-target (renderer texture)
  "Use this function to set a texture as the current rendering target."
  (check-rc (sdl-set-render-target renderer texture)))

(defun get-render-target (renderer)
  (sdl-get-render-target renderer))

(defun render-line (renderer x1 y1 x2 y2)
  "Use this function to draw a line on the current rendering target."
  (check-false (sdl-render-line renderer x1 y1 x2 y2)))

(defun render-lines (renderer points num-points)
  "Pass a pointer to SDL_Point to render connected lines on the current rendering target."
  (check-false (sdl-render-lines renderer points num-points)))

(defun render-point (renderer x y)
  "Use this function to draw a point on the current rendering target."
  (check-false (sdl-render-point renderer x y)))

(defun render-points (renderer points num-points)
  "Use this function to draw multiple points on the current rendering target."
  (check-false (sdl-render-points renderer points num-points)))

(defun render-rect (renderer sdl-rect)
  "Use this function to draw a rectangle on the current rendering target."
  (check-false (sdl-render-rect renderer sdl-rect)))

(defun render-rects (renderer rects num-rects)
  "Use this function to draw some number of rectangles on the current rendering target."
  (check-false (sdl-render-rects renderer rects num-rects)))

(declaim (ftype (function (sdl3-ffi:sdl-renderer sdl3-ffi:sdl-f-rect)) render-fill-rect))
(defun render-fill-rect (renderer f-rect)
  "Use this function to fill a rectangle on the current rendering target with
the drawing color. "
  (check-false (sdl-render-fill-rect renderer f-rect)))

(defun render-fill-rects (renderer rects num-rects)
  "Use this function to fill some number of rectangles on the current
rendering target with the drawing color."
  (check-false (sdl-render-fill-rects renderer rects num-rects)))

(defun render-debug-text (renderer x y text)
  "This function will render a string of text to an SDL_Renderer."
  (when text
    (check-false (sdl-render-debug-text renderer x y text))))

(defun render-set-viewport (renderer sdl-rect)
  "Use this function to set the drawing area for rendering on the current target."
  (check-false (sdl-set-render-viewport renderer sdl-rect)))

(defun render-get-viewport (renderer)
  "Use this function to get the drawing area for the current target."
  (let-rects (rect) (sdl-get-render-viewport renderer (rect &)) rect))

(defun render-clear (renderer)
  "Use this function to clear the current rendering target with the drawing color."
  (check-rc (sdl-render-clear renderer)))

(defun render-present (renderer)
  "Use this function to update the screen with rendering performed."
  (sdl-render-present renderer))

(defun get-renderer-properties (renderer)
  "Get the properties associated with a renderer."
  (check-zero (sdl-get-renderer-properties renderer)))

(defun get-renderer-output-size (renderer)
  "Get the output size in pixels of a rendering context."
  (c-with ((x :int)
           (y :int))
    (sdl-get-render-output-size renderer (x &) (y &))
    (values x y)))

(defun get-texture-properties (texture)
  "Get the properties associated with a texture."
  (check-zero (sdl-get-texture-properties texture)))

(defun texture-width (texture)
  "Return the width as a float when querying the TEXTURE properties."
  (get-float-property (get-texture-properties texture) +prop-texture-width-number+))

(defun texture-height (texture)
  "Return the height as a float when querying the TEXTURE properties."
  (get-float-property (get-texture-properties texture) +prop-texture-height-number+))

(defun update-texture (texture rect pixels pitch)
  "Use this function to update the given texture rectangle with new pixel data."
  (check-rc (sdl-update-texture texture rect pixels pitch)))

(defun create-texture (renderer pixel-format access width height)
  "Use this function to create a texture for a rendering context."
  (check-nullptr (sdl-create-texture renderer
                                     (enum-value 'sdl3-ffi:sdl-pixel-format pixel-format)
                                     (enum-value 'sdl3-ffi:sdl-texture-access access)
                                     width height)))

(defun create-texture-from-surface (renderer surface)
  "Use this function to create a texture from sdl3 surface for a rendering context."
  (check-nullptr (sdl-create-texture-from-surface renderer surface)))

(defun create-texture-with-properties (renderer properties)
  "Create a texture for a rendering context with the specified properties."
  (check-nullptr (sdl-create-texture-with-properties renderer properties)))

(defun set-texture-color-mod (texture r g b)
  "Use this function to set an additional color value multiplied into render copy operations."
  (check-rc (sdl-set-texture-color-mod texture r g b)))

(defun get-texture-color-mod (texture)
  "Use this function to get the additional color value multiplied into render copy operations."
  (c-with ((r :unsigned-char)
           (g :unsigned-char)
           (b :unsigned-char))
    (check-rc (sdl-get-texture-color-mod texture (r &) (g &) (b &)))
    (values r g b)))

(defun set-texture-alpha-mod (texture alpha)
  "Use this function to set an additional alpha value multiplied into render copy operations."
  (check-rc (sdl-set-texture-alpha-mod texture alpha)))

(defun get-texture-alpha-mod (texture)
  "Use this function to get the additional alpha value multiplied into render copy operations."
  (c-with ((alpha :unsigned-char))
    (check-rc (sdl-get-texture-alpha-mod texture (alpha &)))
    alpha))

(defun destroy-texture (texture)
  "Use this function to destroy the specified texture."
  (sdl-destroy-texture texture)
  (invalidate texture))

(defun lock-texture (texture &optional rect)
  "Use this function to lock a portion of the texture for write-only pixel access."
  (c-let ((pixels :pointer :free t)
          (pitch :int :free t))
    (check-rc (sdl-lock-texture texture rect (pixels &) (pitch &)))
    (values pixels pitch)))

(defun unlock-texture (texture)
  "Use this function to unlock a texture, uploading the changes to video memory, if needed. Warning:
See Bug No. 1586 before using this function!"
  (sdl-unlock-texture texture))
