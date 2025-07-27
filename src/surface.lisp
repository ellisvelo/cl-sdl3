(in-package :sdl3)

(defun surface-width (surface)
  (c-ref surface sdl3-ffi:sdl-surface :w))

(defun surface-height (surface)
  (c-ref surface sdl3-ffi:sdl-surface :h))

(defun surface-pixels (surface)
  "Access raw pixel data from a surface object"
  (c-ref surface sdl3-ffi:sdl-surface :pixels))

(defun surface-pitch (surface)
  (c-ref surface sdl3-ffi:sdl-surface :pitch))

(defun surface-format (surface)
  (enum-key '(:enum (sdl3-ffi:sdl-pixel-format))
            (c-ref surface sdl3-ffi:sdl-surface :format)))

(defun create-surface (width height format)
  (sdl-create-surface width height format))

(defun create-surface-from (width height format pixels pitch)
  (sdl-create-surface-from width height format pixels pitch))

(defun destroy-surface (surface)
  (sdl-destroy-surface surface)
  (invalidate surface))

(defun load-bmp (filename)
  (sdl-load-bmp filename))

(defun convert-surface (surface format)
  (sdl-convert-surface surface format))

(defun convert-surface-format (surface pixel-format)
  (check-nullptr
   (sdl-convert-surface surface (enum-value '(:enum (sdl3-ffi:sdl-pixel-format)) pixel-format))))

(defun blit-surface (surface-src src-rect surface-dst dst-rect)
  (sdl-blit-surface surface-src src-rect surface-dst dst-rect))

(defun blit-surface-scaled (surface-src src-rect surface-dst dst-rect scale-mode)
  (sdl-blit-surface-scaled surface-src src-rect surface-dst dst-rect (enum-value '(:enum (sdl3-ffi:sdl-scale-mode)) scale-mode)))

(defun fill-surface-rect (surface-dst rect color)
  (check-false (sdl-fill-surface-rect surface-dst rect color)))

(defun set-surface-color-key (surface flag key)
  "Use this function to set the color key (transparent pixel) in a surface."
  (check-rc (sdl-set-surface-color-key surface flag key)))

(defun get-surface-color-key (surface)
  "Use this function to get the color key (transparent pixel) for a surface."
  (c-let ((key sdl3-ffi:uint32))
    (check-rc (sdl-get-surface-color-key surface (key &)))
    key))

(defun get-surface-colorspace (surface)
  "Get the colorspace used by a surface."
  (sdl-get-surface-colorspace surface))

(defun set-alpha-mod (surface alpha)
  "Use this function to set an additional alpha value used in blit operations."
  (check-rc (sdl-set-surface-alpha-mod surface alpha)))

(defun get-alpha-mod (surface)
  "Use this function to get the additional alpha value used in blit operations."
  (c-let ((alpha sdl3-ffi:uint8))
    (check-rc (sdl-get-surface-alpha-mod surface (alpha &)))
    alpha))

(defun set-color-mod (surface r g b)
  "Use this function to set an additional color value multiplied into blit operations."
  (check-rc (sdl-set-surface-color-mod surface r g b)))

(defun get-color-mod (surface)
  "Use this function to get the additional color value multiplied into blit operations."
  (c-let ((r sdl3-ffi:uint8)
          (g sdl3-ffi:uint8)
          (b sdl3-ffi:uint8))
    (check-rc (sdl-get-surface-color-mod surface (r &) (g &) (b &)))
    (values r g b)))

(defun set-surface-blend-mode (surface blend-mode)
  "Use this function to set the blend mode used for blit operations."
  (check-rc (sdl-set-surface-blend-mode surface blend-mode)))

(defun get-surface-blend-mode (surface)
  "Use this function to get the blend mode used for blit operations."
  (c-with ((blend-mode sdl3-ffi:sdl-blend-mode))
    (check-rc (sdl-get-surface-blend-mode surface (blend-mode &)))
    blend-mode))

(defun scale-surface (surface width height scale-mode)
  "Creates a new surface identical to the existing surface, scaled to the
  desired size. The SCALE-MODE can either be :linear, :nearest, or :invalid."
  (check-nil (sdl-scale-surface surface width height (enum-value 'sdl3-ffi:sdl-scale-mode scale-mode))))
