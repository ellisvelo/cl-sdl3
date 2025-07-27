(in-package #:sdl3)

(defun get-global-properties ()
  "returns a valid property ID on success or 0 on failure"
  (check-zero (sdl-get-global-properties)))

(defun create-properties ()
  "Create a group of properties."
  (check-zero (sdl-create-properties)))

(defun copy-properties (src dst)
   "Copy all the properties from one group of properties to another, with the
 exception of properties requiring cleanup (set using
 SET-POINTER-PROPERTY-WITH-CLEANUP()), which will not be copied. Any property
 that already exists on DST will be overwritten."
  (check-false (sdl-copy-properties src dst)))

(defun lock-properties (props)
  "Obtain a multi-threaded lock for these properties. Other threads will wait
  while trying to lock these properties until they are unlocked. Properties
  must be unlocked before they are destroyed."
  (check-false (sdl-lock-properties props)))

(defun unlock-properties (props)
  "Unlock a group of properties."
  (sdl-unlock-properties props))

(defun set-pointer-property-with-cleanup (props name value cleanup userdate)
  "Set a pointer property in a group of properties with a cleanup function
 that is called when the property is deleted. The cleanup function is also called if setting the property fails for any reason."
  (check-false (sdl-set-pointer-property-with-cleanup props name value cleanup userdate)))

(defun set-pointer-property (props name value)
  "Set a pointer property in a group of properties."
  (check-false (sdl-set-pointer-property props name value)))

(defun set-string-property (props name value)
  "Set a string property in a group of properties."
  (check-false (sdl-set-string-property props name value)))

(defun set-number-property (props name value)
  "Set an integer property in a group of properties."
  (check-false (sdl-set-number-property props name value)))

(defun set-float-property (props name value)
  "Set a floating point property in a group of properties."
  (check-false (sdl-set-float-property props name value)))

(defun set-boolean-property (props name value)
  "Set a boolean property in a group of properties."
  (check-false (sdl-set-boolean-property props name (boolean-to-bool value))))

(defun has-property-p (props name)
  "Return whether a property exists in a group of properties."
  (sdl-true-p (sdl-has-property props name)))

(defun get-property-type (props name)
  "Get the type of a property in a group of properties."
  (autowrap:enum-key 'sdl3-ffi:sdl-property-type (sdl-get-property-type props name)))

(defun get-pointer-property (props name &optional default-value)
  "Get a pointer property from a group of properties."
  (sdl-get-pointer-property props name default-value))

(defun get-string-property (props name &optional default-value)
  "Get a string property from a group of properties."
  (sdl-get-string-property props name default-value))

(defun get-number-property (props name &optional (default-value 0))
  "Get a number property from a group of properties."
  (sdl-get-number-property props name default-value))

(defun get-float-property (props name &optional (default-value 0.0))
  "Get a floating point property from a group of properties."
  (sdl-get-float-property props name default-value))

(defun get-boolean-property (props name &optional (default-value nil))
  "Get a boolean property from a group of properties."
  (sdl-true-p (sdl-get-boolean-property props name (boolean-to-bool default-value))))

(defun clear-property (props name)
  "Clear a property from a group of properties."
  (sdl-clear-property props name))

(defun get-property-value (props name &optional (default-value nil))
  "Get the value of a property by examining the property type."
  (ecase (get-property-type props name)

    (:pointer
     (get-pointer-property props name default-value))

    (:string
     (get-string-property props name default-value))

    (:number
     (get-number-property props name (or default-value 0)))

    (:float
     (get-float-property props name (or default-value 0.0)))

    (:boolean
     (get-boolean-property props name default-value))

    (:invalid nil)))

(defun enumerate-properties (props callback userdata)
  "Enumerate the properties contained in a group of properties. The callback
function is called for each property in the group of properties. The properties
are locked during enumeration."
  (sdl-enumerate-properties props callback userdata))

(defvar *props-hash*)

;; The callback invoked when traversing the properties
(autowrap:defcallback get-all-properties-cb :int ((user-data :pointer) (props sdl3-ffi:sdl-properties-id) (name (:pointer :char)))
  (declare (ignore user-data))
  (let* ((prop-name (cffi:foreign-string-to-lisp name))
	 (prop-value (get-property-value props prop-name)))
    (setf (gethash prop-name *props-hash*) prop-value)
    0))

(defun get-all-properties (props)
  "Return a HASHTABLE containing the keys and values for the SDL Properties."
  (let ((*props-hash* (make-hash-table :test #'equal)))
    (enumerate-properties props (cffi:callback get-all-properties-cb) nil)
    *props-hash*))

(defun destroy-properties (props)
  "Destroy a group of properties."
  (sdl-destroy-properties props))
