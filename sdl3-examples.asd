(asdf:defsystem #:sdl3-examples
  :description "Simple examples to demonstrate common usage of SDL3."
  :author "Chip Collier <photex@lofidelitygames.com> and Shawn Ellis"
  :license "MIT"
  :depends-on (:sdl3 :cl-opengl)
  :pathname "examples"
  :serial t
  :components ((:file "package")
	       (:file "square")
               (:file "basic-test")
               (:file "camera-test")
               (:file "gamepad-test")
               (:file "joystick-test")
               (:file "properties-test")
               (:file "renderer-test")))
