(in-package :sdl3)

(cffi:define-foreign-library libsdl3
  (:darwin (:or (:framework "SDL3") (:default "libSDL3.0")))
  (:unix (:or "libSDL3-2.0.so.0" "libSDL3.so.0" "libSDL3"))
  (:windows "SDL3.dll")
  (t (:default "libSDL3")))

(cffi:use-foreign-library libsdl3)
