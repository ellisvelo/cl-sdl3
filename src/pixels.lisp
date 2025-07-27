(in-package #:sdl3)

(autowrap:define-enum-from-constants (sdl-pixel-type)
  sdl3-ffi:+sdl-pixeltype-unknown+
  sdl3-ffi:+sdl-pixeltype-index1+
  sdl3-ffi:+sdl-pixeltype-index4+
  sdl3-ffi:+sdl-pixeltype-index8+
  sdl3-ffi:+sdl-pixeltype-packed8+
  sdl3-ffi:+sdl-pixeltype-packed16+
  sdl3-ffi:+sdl-pixeltype-packed32+
  sdl3-ffi:+sdl-pixeltype-arrayu8+
  sdl3-ffi:+sdl-pixeltype-arrayu16+
  sdl3-ffi:+sdl-pixeltype-arrayu32+
  sdl3-ffi:+sdl-pixeltype-arrayf16+
  sdl3-ffi:+sdl-pixeltype-arrayf32+)

(autowrap:define-enum-from-constants (sdl-bitmap-order)
  sdl3-ffi:+sdl-bitmaporder-none+
  sdl3-ffi:+sdl-bitmaporder-4321+
  sdl3-ffi:+sdl-bitmaporder-1234+)

(autowrap:define-enum-from-constants (sdl-packed-order)
  sdl3-ffi:+sdl-packedorder-none+
  sdl3-ffi:+sdl-packedorder-xrgb+
  sdl3-ffi:+sdl-packedorder-rgbx+
  sdl3-ffi:+sdl-packedorder-argb+
  sdl3-ffi:+sdl-packedorder-rgba+
  sdl3-ffi:+sdl-packedorder-xbgr+
  sdl3-ffi:+sdl-packedorder-bgrx+
  sdl3-ffi:+sdl-packedorder-abgr+
  sdl3-ffi:+sdl-packedorder-bgra+)

(autowrap:define-enum-from-constants (sdl-array-order)
  sdl3-ffi:+sdl-arrayorder-none+
  sdl3-ffi:+sdl-arrayorder-rgb+
  sdl3-ffi:+sdl-arrayorder-rgba+
  sdl3-ffi:+sdl-arrayorder-argb+
  sdl3-ffi:+sdl-arrayorder-bgr+
  sdl3-ffi:+sdl-arrayorder-bgra+
  sdl3-ffi:+sdl-arrayorder-abgr+)

(autowrap:define-enum-from-constants (sdl-packed-layout)
  sdl3-ffi:+sdl-packedlayout-none+
  sdl3-ffi:+sdl-packedlayout-332+
  sdl3-ffi:+sdl-packedlayout-4444+
  sdl3-ffi:+sdl-packedlayout-1555+
  sdl3-ffi:+sdl-packedlayout-5551+
  sdl3-ffi:+sdl-packedlayout-565+
  sdl3-ffi:+sdl-packedlayout-8888+
  sdl3-ffi:+sdl-packedlayout-2101010+
  sdl3-ffi:+sdl-packedlayout-1010102+)

(defun map-rgb (&key pixel-format palette r g b)
  (sdl-map-rgb pixel-format palette r g b))

(defun get-pixel-format-name (format-integer)
  "Returns the human readable name for a surface's pixel format, useful for
debugging."
  (sdl3-ffi.functions:sdl-get-pixel-format-name format-integer))

(defun get-pixel-format-for-masks (&key bits-per-pixel r-mask g-mask b-mask a-mask)
  "Convert a bpp value and RGBA masks to an enumerated pixel format."
  (sdl-get-pixel-format-for-masks bits-per-pixel r-mask g-mask b-mask a-mask))
