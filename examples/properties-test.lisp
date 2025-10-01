(in-package :sdl3-examples)

(require :sdl3)

(defun test-get-string-property (renderer props name)
  (let ((text (format nil "String prop: ~a" (sdl3:get-string-property props name ))))
    (sdl3:render-debug-text renderer 200.0 200.0 text)))

(defun test-get-boolean-property (renderer props name)
  (let ((text (format nil "Boolean prop: ~a" (sdl3:get-boolean-property props name ))))
    (sdl3:render-debug-text renderer 200.0 220.0 text)))

(defun test-get-float-property (renderer props name)
  (let ((text (format nil "Float prop: ~a" (sdl3:get-float-property props name))))
    (sdl3:render-debug-text renderer 200.0 240.0 text)))

(defun test-get-pointer-property (renderer)
  (let* ((props (sdl3:get-renderer-properties renderer))
         (text (format nil "Pointer prop: ~a" (sdl3:get-pointer-property props sdl3:+prop-renderer-window-pointer+))))
    (sdl3:render-debug-text renderer 200.0 260.0 text)))

(defun properties-test ()
  "Test various property related functions."
  (sdl3:with-init (:video)
    (sdl3:with-window (window :title "SDL3 Properties Demo")
      (sdl3:with-renderer (renderer window)
        ;; Set the vsync
        (sdl3:set-render-vsync renderer t)

        (let ((props (sdl3:create-properties)))
          (sdl3:set-string-property props "hello" "hello world")
          (sdl3:set-boolean-property props "true-false" t)
          (sdl3:set-float-property props "float-prop" 4.5)

          (let ((all-props (sdl3:get-all-properties props)))
            (format t "Properties: ~a~%" all-props)
            (format t "Float: ~a~%" (gethash "float-prop" all-props)))

          (format t "True/False: ~a~%" (sdl3:get-property-value props "true-false"))

          (sdl3:with-event-loop (:method :poll)
            (:key-down (:scancode scancode)
                       (when (sdl3:scancode= scancode :escape)
                         (sdl3:push-event :quit)))

            (:idle ()
                   (sdl3:set-render-draw-color renderer 0 0 0 255)
                   (sdl3:render-clear renderer)
                   (sdl3:set-render-draw-color renderer 255 255 255 255)
                   (test-get-string-property renderer props "hello")
                   (test-get-boolean-property renderer props "true-false")
                   (test-get-float-property renderer props "float-prop")
                   (test-get-pointer-property renderer)
                   (sdl3:render-present renderer))

            (:quit ()
                   (sdl3:destroy-properties props)
                   t)))))))
