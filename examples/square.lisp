(in-package :sdl3-examples)

(defparameter *gamepad-dead-zone* 4000)

(defclass square ()
  ((x
    :initarg :x
    :initform 0.0
    :accessor square-x)
   (y
    :initarg :y
    :initform 0.0
    :accessor square-y)
   (w
    :initarg :w
    :initform 20.0
    :accessor square-width)
   (h
    :initarg :h
    :initform 20.0
    :accessor square-height)
   (vx
    :initarg :vx
    :initform 0.0
    :accessor square-vx)
   (vy
    :initarg :vy
    :initform 0.0
    :accessor square-vy)
   (max-x
    :initarg :max-x
    :initform 0.0
    :accessor square-max-x)
   (max-y
    :initarg :max-y
    :initform 0.0
    :accessor square-max-y)
   (max-vx
    :initarg :max-vx
    :initform 2.0
    :accessor square-max-vx)
   (max-vy
    :initarg :max-vy
    :initform 2.0
    :accessor square-max-vy)))

(defun limit-value (value min-value max-value)
  (cond ((< value min-value)
         min-value)

        ((> value max-value)
         max-value)

        (t value)))

(defun move-square (square)
  (with-slots (x y w h max-x max-y vx vy) square
    (incf x vx)
    (setf x (limit-value x 0.0 max-x))

    (incf y vy)
    (setf y (limit-value y 0.0 max-y))))

(defun render-square (renderer square)
  (with-slots (x y w h) square
    (sdl3:with-f-rects ((rect x y w h))
      (sdl3:render-fill-rect renderer rect))))

(defun handle-gamepad-motion (square axis-id value)
  (with-slots (vx vy max-vx max-vy) square
    (cond ((= sdl3:+gamepad-axis-leftx+ axis-id)
           (cond ((> value *gamepad-dead-zone*)
                  (incf vx 0.5))

                 ((< value (- *gamepad-dead-zone*))
                  (decf vx 0.5))))

          ((= sdl3:+gamepad-axis-lefty+ axis-id)
           (cond ((> value *gamepad-dead-zone*)
                  (incf vy 0.5))

                 ((< value (- *gamepad-dead-zone*))
                  (decf vy 0.5)))))

    (setf vx (limit-value vx (- max-vx) max-vx))
    (setf vy (limit-value vy (- max-vy) max-vy))))
