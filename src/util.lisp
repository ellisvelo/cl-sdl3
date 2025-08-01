(in-package :sdl3)

;; no export
(defun unique-pairs (list)
  "Return all unique pair combinations of the list."
  (mapcon (lambda (x)
            (mapcar (lambda (y)
                      (list (car x) y))
                    (cdr x)))
          list))

(defmacro define-struct-accessors ((prefix foreign-struct) &body fields)
  `(progn
     ,@(loop :for field :in fields
             :as suffix = (typecase field
                            (list (car field))
                            (symbol field))
             :as field-name = (typecase field
                                (list (cadr field))
                                (symbol field))
             :as name = (symbolicate prefix "-" suffix)
             :collect
             `(defun ,name (,prefix) (c-ref ,prefix ,foreign-struct ,field-name))
             :collect
             `(defun (setf ,name) (v ,prefix)
                (setf (c-ref ,prefix ,foreign-struct ,field-name) v)))))

(defun sdl-array-to-list (sdl-pointer count)
  "Convert an array in SDL to a list"
  (let ((results '()))
    (dotimes (i count)
      (push (autowrap:c-aref sdl-pointer i :int) results))
    (sdl-free (autowrap:make-wrapper :ptr sdl-pointer))
    (reverse results)))
