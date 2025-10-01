(cl:in-package :sdl3-ffi)

(autowrap:c-include
 '(sdl3 autowrap-spec "SDL3.h")
 :accessor-package :sdl3-ffi.accessors
 :function-package :sdl3-ffi.functions
 :spec-path '(sdl3 autowrap-spec)
 :exclude-sources ("/usr/local/lib/clang/([^/]*)/include/(?!stddef.h)"
                   "/usr/include/"
                   "/usr/include/arm-linux-gnueabihf"
                   "/usr/include/X11/")
 :include-sources ("sys/_types/_int8_t.h"
                   "sys/_types/_int16_t.h"
                   "sys/_types/_int32_t.h"
                   "sys/_types/_int64_t.h"
                   "sys/_types/_u_int8_t.h"
                   "sys/_types/_u_int16_t.h"
                   "sys/_types/_u_int32_t.h"
                   "sys/_types/_u_int64_t.h"
                   "sys/_types/_size_t.h"
                   "sys/_types/_wchar_t.h"
                   "_types/_uint8_t.h"
                   "_types/_uint16_t.h"
                   "_types/_uint32_t.h"
                   "_types/_uint64_t.h"
                   "stdint.h"
                   "bits/types.h"
                   "bits/types/FILE.h"
                   "sys/types.h"
                   "bits/stdint"
                   "machine/_types.h"
                   "SDL3")
 :sysincludes `,(cl:append
                 (uiop:split-string (uiop:getenv "EXTRA_INCLUDES") :separator " ")
                 #+openbsd (cl:list "/usr/X11R6/include")
                 #+(and unix (not darwin))
                 (cl:list "/usr/lib/clang/17/include/" "/usr/include/"))
 :exclude-definitions ("SDL_main"
                       "SDL_LogMessageV"
                       "SDL_vsnprintf"
                       "_inline$"
                       "^_mm_")
 :include-definitions ("^XID$" "^Window$" "^Display$" "^_XDisplay$")
 :symbol-exceptions (("SDL_Log" . "SDL-LOGGER")
                     ("SDL_log" . "SDL-LOGN")
                     ("SDL_GLContext" . "SDL-GLCONTEXT")
                     ("SDL_GLattr" . "SDL-GLATTR")
                     ("SDL_GLprofile" . "SDL-GLPROFILE")
                     ("SDL_GLcontextFlag" . "SDL-GLCONTEXT-FLAG")
                     ("SDL_SysWMinfo" . "SDL-SYSWM-INFO")
                     ("SDL_TRUE" . "TRUE")
                     ("SDL_FALSE" . "FALSE"))
 :no-accessors cl:t
 :release-p cl:t)
