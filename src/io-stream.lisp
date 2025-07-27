;; SDL3 offers an abstract interface for I/O streams. This file defines bindings
;; operations on SDL_IOStream including creating SDL_IOStream structures from
;; files and closing them
(in-package :sdl3)

(defun %sdl-io-close (sdl-io-stream-ptr)
  (cffi:foreign-funcall-pointer
   (plus-c:c-ref sdl-io-stream-ptr sdl3-ffi:sdl-io-stream-interface :close)
   ()
   :pointer sdl-io-stream-ptr
   :int))

(defun close-io (sdl-io-stream)
  "Flush the file represented by the sdl-io-stream object and free the memory
associated with it. Returns 0 if the file is successfully flushed and -1
otherwise. Even if the file fails to flush the memory is freed and pointer is
invalid"
  (%sdl-io-close (autowrap:ptr sdl-io-stream))
  (autowrap:invalidate sdl-io-stream))

(defun io-from-file (file-name mode)
  "Create an io-stream structure from a given file name in a given mode."
  (check-nullptr (sdl-io-from-file file-name mode)))
