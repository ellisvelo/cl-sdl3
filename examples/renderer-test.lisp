(in-package :sdl3-examples)

(require :sdl3)

(defun test-render-clear (renderer)
  (sdl3:set-render-draw-color renderer 0 0 0 255)
  (sdl3:render-clear renderer))

(defun test-render-hello (renderer)
  (sdl3:set-render-draw-color renderer 255 0 0 255)
  ;;
  (sdl3:render-line renderer 20.0 20.0 20.0 100.0)
  (sdl3:render-line renderer 20.0 60.0 60.0 60.0)
  (sdl3:render-line renderer 60.0 20.0 60.0 100.0)
  ;; E
  (sdl3:render-line renderer 80.0 20.0 80.0 100.0)
  (sdl3:render-line renderer 80.0 20.0 120.0 20.0)
  (sdl3:render-line renderer 80.0 60.0 120.0 60.0)
  (sdl3:render-line renderer 80.0 100.0 120.0 100.0)
  ;; L
  (sdl3:render-line renderer 140.0 20.0 140.0 100.0)
  (sdl3:render-line renderer 140.0 100.0 180.0 100.0)
  ;; L
  (sdl3:render-line renderer 200.0 20.0 200.0 100.0)
  (sdl3:render-line renderer 200.0 100.0 240.0 100.0)
  ;; O
  (sdl3:render-line renderer 260.0 20.0 260.0 100.0)
  (sdl3:render-line renderer 260.0 100.0 300.0 100.0)
  (sdl3:render-line renderer 300.0 20.0 300.0 100.0)
  (sdl3:render-line renderer 260.0 20.0 300.0 20.0))

(defun test-render-lines (renderer)
  (sdl3:with-points ((a 200 200)
                     (b 300 400)
                     (c 400 200))
    (sdl3:set-render-draw-color renderer 0 0 255 255)
    (multiple-value-bind (points num) (sdl3:points* a b c)
      (sdl3:render-lines renderer points num))))

(defun test-render-f-lines (renderer)
  (sdl3:with-f-points ((a 200.0 200.0)
                       (b 300.0 100.0)
                       (c 400.0 200.0))
    (sdl3:set-render-draw-color renderer 0 0 255 255)
    (multiple-value-bind (points num) (sdl3:f-points* a b c)
      (sdl3:render-lines renderer points num))))

(defun test-render-points (renderer)
  (sdl3:with-points ((a (random 800) (random 800))
                     (b (random 800) (random 800))
                     (c (random 800) (random 800)))
    (sdl3:set-render-draw-color renderer 0 255 0 255)
    (multiple-value-bind (points num) (sdl3:points* a b c)
      (sdl3:render-points renderer points num))))

(defun test-render-rect (renderer)
  (sdl3:render-rect renderer (sdl3:make-rect 400 400 35 35)))

(defun test-render-rects (renderer)
  (multiple-value-bind (rects num)
      (apply #'sdl3:rects*
             (loop :for x :upto 5
                   :for y :upto 5
                   :collect (sdl3:make-rect (+ 400 (* x 10)) (+ 200 (* y 10)) 8 8)))
    (sdl3:render-rects renderer rects num)))

(defun test-render-fill-rect (renderer)
  (sdl3:render-fill-rect renderer (sdl3:make-f-rect 445.0 400.0 35.0 35.0)))

(defun test-render-fill-rects (renderer)
  (multiple-value-bind (rects num)
      (apply #'sdl3:rects*
             (loop :for x :upto 5
                   :collect (sdl3:make-f-rect (+ 500.0 (* x 10)) 400.0 8.0 8.0)))
    (sdl3:set-render-draw-color renderer 255 0 255 255)
    (sdl3:render-fill-rects renderer rects num)))

(defun test-render-debug-text (renderer)
  (sdl3:render-debug-text renderer 350.0 50.0 "Common Lisp"))

(defun test-render-properties (renderer)
  (let* ((props (sdl3:get-renderer-properties renderer))
         (max-texture-size (sdl3:get-number-property props sdl3:+prop-renderer-max-texture-size-number+))
         (text (format nil "Max Texture Size: ~a" max-texture-size)))
    (sdl3:render-debug-text renderer 350.0 150.0 text)))

(defun test-render-output-size (renderer)
  (multiple-value-bind (x y) (sdl3:get-renderer-output-size renderer)
    (let ((text (format nil "Output size: ~a ~a" x y)))
      (sdl3:render-debug-text renderer 350.0 165.0 text))))

(defun renderer-test ()
  "Test the SDL_render.h API"
  (sdl3:with-init (:everything)
    (sdl3:with-window (win :title "SDL3 Renderer API Demo")
      (sdl3:with-renderer (renderer win)
        (sdl3:with-event-loop (:method :poll)
          (:key-up (:scancode scancode)
                   (when (sdl3:scancode= scancode :escape)
                     (sdl3:push-event :quit)))
          (:idle
           ()
           (test-render-clear renderer)
           (test-render-hello renderer)
           (test-render-lines renderer)
           (test-render-f-lines renderer)
           (test-render-points renderer)
           (test-render-rect renderer)
           (test-render-rects renderer)
           (test-render-fill-rect renderer)
           (test-render-fill-rects renderer)
           (test-render-debug-text renderer)
           (test-render-properties renderer)
           (test-render-output-size renderer)

           (sdl3:render-present renderer)
           (sdl3:delay 33))
          (:quit () t))))))
