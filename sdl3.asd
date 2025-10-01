(asdf:defsystem #:sdl3
  :description "Bindings for SDL3 using c2ffi."
  :author "Michael Fiano <mail@mfiano.net>, Chip Collier <photex@lofidelitygames.com>, Ryan Pavlik <rpavlik@gmail.com>, Peter Keller <psilord@cs.wisc.edu>"
  :license "MIT"
  :depends-on (:alexandria
               :cl-autowrap
               :cl-plus-c
               :cl-ppcre
               :trivial-channels
               :trivial-features
               :float-features)
  :pathname "src"
  :serial t
  :components
  ((:module autowrap-spec
    :pathname "spec"
    :components
    ((:static-file "SDL3.h")
     (:static-file "SDL3.aarch64-pc-linux-gnu.spec")
     (:static-file "SDL3.aarch64-apple-darwin9.spec")
     (:static-file "SDL3.aarch64-unknown-linux-android.spec")
     (:static-file "SDL3.arm-pc-linux-gnu.spec")
     (:static-file "SDL3.arm-unknown-linux-androideabi.spec")
     (:static-file "SDL3.i386-unknown-freebsd.spec")
     (:static-file "SDL3.i386-unknown-openbsd.spec")
     (:static-file "SDL3.i686-apple-darwin9.spec")
     (:static-file "SDL3.i686-pc-linux-gnu.spec")
     (:static-file "SDL3.i686-pc-windows-msvc.spec")
     (:static-file "SDL3.i686-unknown-linux-android.spec")
     (:static-file "SDL3.powerpc64-pc-linux-gnu.spec")
     (:static-file "SDL3.powerpc64le-pc-linux-gnu.spec")
     (:static-file "SDL3.x86_64-apple-darwin9.spec")
     (:static-file "SDL3.x86_64-pc-linux-gnu.spec")
     (:static-file "SDL3.x86_64-pc-windows-msvc.spec")
     (:static-file "SDL3.x86_64-unknown-freebsd.spec")
     (:static-file "SDL3.x86_64-unknown-linux-android.spec")
     (:static-file "SDL3.x86_64-unknown-openbsd.spec")))
   (:file "package")
   (:file "library")
   (:file "autowrap")
   (:file "util")
   (:file "constants")
   (:file "sdl3")
   (:file "hints")
   (:file "rect")
   (:file "video")
   (:file "events")
   (:file "keyboard")
   (:file "mouse")
   (:file "syswm")
   (:file "joystick")
   (:file "gamepad")
   (:file "haptic")
   (:file "timer")
   (:file "audio")
   (:file "platform")
   (:file "pixels")
   (:file "surface")
   (:file "io-stream")
   (:file "properties")
   (:file "camera")
   (:file "render" :depends-on ("rect"))))
