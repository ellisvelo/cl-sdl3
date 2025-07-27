;;;; package.lisp

(push :sdl3 *features*)

(uiop:define-package #:sdl3-ffi (:use))
(uiop:define-package #:sdl3-ffi.accessors (:use))
(uiop:define-package #:sdl3-ffi.functions
  (:use)
  (:export #:sdl-quit))

(defpackage #:sdl3
  (:use #:cl
        #:alexandria
        #:autowrap.minimal
        #:plus-c
        #:sdl3-ffi.accessors
        #:sdl3-ffi.functions
        #:trivial-channels)
  (:import-from
   #:cffi
   #:mem-ref
   #:with-foreign-objects
   #:with-foreign-object
   #:foreign-alloc
   #:foreign-free
   #:null-pointer-p)
  (:shadow #:sdl-error)
  (:export
   #:init
   #:init*
   #:quit
   #:quit*
   #:was-init
   #:with-init
   #:in-main-thread
   #:revision
   #:version
   #:version-wrapped
   #:make-this-thread-main
   #:get-error
   #:sdl-error
   #:sdl-rc-error

   ;; audio.lisp
   #:get-audio-playback-devices
   #:get-audio-recording-devices
   #:get-audio-device-name

   ;; camera.lisp
   #:get-num-camera-drivers
   #:get-camera-driver
   #:get-current-camera-driver
   #:get-cameras
   #:get-camera-supported-formats
   #:get-camera-name
   #:get-camera-id
   #:get-camera-format
   #:get-camera-properties
   #:get-camera-permission-state
   #:open-camera
   #:acquire-camera-frame
   #:release-camera-frame
   #:close-camera

   ;; hints.lisp
   #:get-hint
   #:set-hint

   ;; video.lisp
   #:get-num-video-drivers
   #:get-video-driver
   #:get-current-video-driver
   #:get-num-video-displays
   #:get-display-name
   #:get-num-display-modes
   #:get-primary-display
   #:get-current-display-mode
   #:get-display-mode
   #:get-display-bounds
   #:windowpos-undefined
   #:windowpos-centered
   #:windowpos-from-coord
   #:create-window
   #:destroy-window
   #:with-window
   #:hide-window
   #:show-window
   #:maximize-window
   #:minimize-window
   #:raise-window
   #:restore-window
   #:update-window
   #:set-window-title
   #:set-window-fullscreen
   #:set-window-size
   #:set-window-position
   #:get-window-title
   #:get-window-size
   #:get-window-aspect-ratio
   #:get-window-surface
   #:get-window-position
   #:get-window-flags
   #:get-window-pixel-format
   #:get-window-id
   #:get-window-display-index
   #:enable-screensaver
   #:disable-screensaver
   #:screensaver-enabled-p
   #:gl-create-context
   #:gl-delete-context
   #:with-gl-context
   #:gl-extension-supported-p
   #:gl-make-current
   #:gl-get-swap-interval
   #:gl-set-swap-interval
   #:gl-swap-window
   #:gl-get-attr
   #:gl-get-attrs
   #:gl-set-attr
   #:gl-set-attrs
   #:gl-get-proc-address
   #:with-everything

   ;; events.lisp
   #:new-event
   #:free-event
   #:register-user-event-type
   #:with-sdl-event
   #:get-event-type
   #:pump-events
   #:push-event
   #:push-user-event
   #:push-quit-event
   #:next-event
   #:with-event-loop
   #:expand-handler

   ;; keyboard.lisp
   #:keysym-slot-value
   #:key-down-p
   #:key-up-p
   #:scancode-value
   #:scancode
   #:scancode-symbol
   #:scancode-key-to-value
   #:get-mod-state
   #:mod-value
   #:sym-value
   #:scancode=
   #:mod-keywords
   #:mod-value-p
   #:mod-key-state-p
   #:keyboard-state-p
   #:get-key-from-scancode
   #:get-key-name
   #:scancode-name
   #:scancode-key-name
   #:start-text-input
   #:stop-text-input

   ;; mouse.lisp
   #:warp-mouse-in-window
   #:hide-cursor
   #:show-cursor
   #:cursor-visible-p
   #:set-relative-mouse-mode
   #:relative-mouse-mode-p
   #:toggle-relative-mouse-mode
   #:mouse-state
   #:mouse-state-p
   #:get-global-mouse-state
   #:global-mouse-state-p

   ;; joystick.lisp
   #:update-joysticks
   #:get-joysticks
   #:open-joystick
   #:close-joystick
   #:get-joystick-name
   #:get-joystick-name-for-id
   #:get-joystick-from-id
   #:get-num-joystick-hats
   #:get-num-joystick-axes
   #:get-num-joystick-balls
   #:get-num-joystick-buttons
   #:get-joystick-id
   #:get-joystick-axis
   #:get-joystick-connection-state
   #:get-joystick-hat
   #:get-joystick-properties
   #:rumble-joystick
   #:joystick-has-rumble-p

   ;; gamepad.lisp
   #:get-gamepads
   #:gamepad-p
   #:get-gamepad-name-for-id
   #:open-gamepad
   #:close-gamepad
   #:gamepad-connected-p
   #:add-gamepad-mapping
   #:get-gamepad-joystick
   #:get-gamepad-properties
   #:gamepad-has-rumble-p
   #:get-gamepad-joystick-id
   #:get-gamepad-from-id
   #:add-gamepad-mappings-from-file
   #:get-gamepad-name
   #:get-gamepad-axis
   #:get-gamepad-axis-from-string
   #:get-gamepad-mapping
   #:get-gamepad-button
   #:get-gamepad-button-from-string
   #:get-gamepad-string-for-button
   #:get-gamepad-button-label
   #:rumble-gamepad

   ;; properties.lisp
   #:get-global-properties
   #:create-properties
   #:copy-properties
   #:lock-properties
   #:unlock-properties
   #:set-pointer-property-with-cleanup
   #:set-pointer-property
   #:set-string-property
   #:set-number-property
   #:set-float-property
   #:set-boolean-property
   #:has-property-p
   #:get-property-type
   #:get-pointer-property
   #:get-string-property
   #:get-number-property
   #:get-float-property
   #:get-boolean-property
   #:clear-property
   #:enumerate-properties
   #:get-property-value
   #:get-all-properties
   #:destroy-properties

   ;; rect.lisp
   #:make-point
   #:make-f-point
   #:copy-point
   #:copy-into-point
   #:free-point
   #:with-points
   #:with-f-points
   #:points*
   #:f-points*
   #:c-rect
   #:c-rects
   #:make-rect
   #:copy-rect
   #:copy-into-rect
   #:free-rect
   #:let-rects
   #:with-rects
   #:rects*
   #:rect-empty
   #:rect-equals
   #:has-rect-intersection
   #:rect-intersection
   #:has-rect-intersection-float-p
   #:rect-and-line-intersection
   #:rect-and-line-intersection-float
   #:rect-union
   #:c-f-rect
   #:c-f-rects
   #:make-f-rect
   #:copy-f-rect
   #:copy-into-f-rect
   #:free-f-rect
   #:let-f-rects
   #:with-f-rects
   #:f-rects*
   #:f-rect-empty
   #:f-rect-equals

   ;; render.lisp
   #:get-num-render-drivers
   #:create-window-and-renderer
   #:create-renderer
   #:create-renderer-with-properties
   #:create-software-renderer
   #:destroy-renderer
   #:with-renderer
   #:get-renderer
   #:render-texture
   #:render-texture-rotated
   #:get-render-vsync
   #:set-render-vsync
   #:set-render-draw-color
   #:get-render-draw-color
   #:set-texture-blend-mode
   #:get-texture-blend-mode
   #:set-render-draw-blend-mode
   #:set-render-target
   #:get-render-target
   #:render-line
   #:render-lines
   #:render-point
   #:render-points
   #:render-rect
   #:render-rects
   #:render-fill-rect
   #:render-fill-rects
   #:render-debug-text
   #:render-set-viewport
   #:render-get-viewport
   #:render-clear
   #:render-present
   #:get-renderer-properties
   #:get-renderer-output-size
   #:get-texture-properties
   #:texture-width
   #:texture-height
   #:update-texture
   #:create-texture
   #:create-texture-from-surface
   #:create-texture-with-properties
   #:set-texture-color-mod
   #:get-texture-color-mod
   #:set-texture-alpha-mod
   #:get-texture-alpha-mod
   #:destroy-texture
   #:lock-texture
   #:unlock-texture

   ;; haptic.lisp
   #:get-haptics
   #:is-joystick-haptic-p
   #:is-mouse-haptic-p
   #:open-haptic
   #:open-haptic-from-joystick
   #:open-haptic-from-mouse
   #:close-haptic
   #:get-haptic-id
   #:get-haptic-from-id
   #:haptic-rumble-supported-p
   #:init-haptic-rumble
   #:play-haptic-rumble
   #:stop-haptic-rumble

   ;; pixels.lisp
   #:map-rgb
   #:get-pixel-format-name
   #:get-pixel-format-for-masks

   ;; surface.lisp
   #:surface-width
   #:surface-height
   #:surface-pixels
   #:surface-pitch
   #:surface-format
   #:create-surface
   #:create-surface-from
   #:destroy-surface
   #:load-bmp
   #:convert-surface
   #:convert-surface-format
   #:blit-surface
   #:blit-surface-scaled
   #:fill-surface-rect
   #:set-surface-color-key
   #:get-surface-color-key
   #:get-surface-colorspace
   #:set-alpha-mod
   #:get-alpha-mod
   #:set-color-mod
   #:get-color-mod
   #:set-surface-blend-mode
   #:get-surface-blend-mode
   #:scale-surface

   ;; timer.lisp
   #:delay
   #:get-ticks
   #:get-performance-counter
   #:get-performance-frequency
   #:add-timer
   #:remove-timer

   ;; platform.lisp
   #:platform
   #:cpu-cache-line-size
   #:cpu-count
   #:mmx-p #:alti-vec-p #:rdtsc-p
   #:sse-p #:sse2-p #:sse3-p #:sse41-p #:sse42-p
   #:power-info

   ;; syswm.lisp
   #:get-window-wm-info

   ;;io-stream.lisp
   :io-from-file
   :close-io

   ;; Utility
   #:sdl-ptr

   ;; Conditions
   #:sdl-continue
   #:sdl-quit

   ;; Constant pixel formats
   #:+pixelformat-unknown+
   #:+pixelformat-index1lsb+
   #:+pixelformat-index1msb+
   #:+pixelformat-index2lsb+
   #:+pixelformat-index12msb+
   #:+pixelformat-index4lsb+
   #:+pixelformat-index14msb+
   #:+pixelformat-index8+
   #:+pixelformat-rgb332+
   #:+pixelformat-xrgb4444+
   #:+pixelformat-xbgr4444+
   #:+pixelformat-xrgb1555+
   #:+pixelformat-xbgr1555+
   #:+pixelformat-argb4444+
   #:+pixelformat-rgba4444+
   #:+pixelformat-abgr4444+
   #:+pixelformat-bgra4444+
   #:+pixelformat-argb1555+
   #:+pixelformat-argba5551+
   #:+pixelformat-abgr1555+
   #:+pixelformat-bgra5551+
   #:+pixelformat-rgb565+
   #:+pixelformat-bgr565+
   #:+pixelformat-rgb24+
   #:+pixelformat-bgr24+
   #:+pixelformat-xrgb8888+
   #:+pixelformat-rgbx8888+
   #:+pixelformat-xbgr8888+
   #:+pixelformat-bgrx8888+
   #:+pixelformat-argb8888+
   #:+pixelformat-rgba8888+
   #:+pixelformat-abgr8888+
   #:+pixelformat-bgra8888+
   #:+pixelformat-xrgb2101010+
   #:+pixelformat-xbgr2101010+
   #:+pixelformat-argb2101010+
   #:+pixelformat-abgr2101010+
   #:+pixelformat-rgb48+
   #:+pixelformat-bgr48+
   #:+pixelformat-rgba64+
   #:+pixelformat-argb64+
   #:+pixelformat-bgra64+
   #:+pixelformat-abgr64+
   #:+pixelformat-rgb48-float+
   #:+pixelformat-bgr48-float+
   #:+pixelformat-rgba64-float+
   #:+pixelformat-argb64-float+
   #:+pixelformat-bgra64-float+
   #:+pixelformat-abgr64-float+
   #:+pixelformat-rgb96-float+
   #:+pixelformat-bgr96-float+
   #:+pixelformat-rgba128-float+
   #:+pixelformat-argb128-float+
   #:+pixelformat-bgra128-float+
   #:+pixelformat-abgr128-float+
   #:+pixelformat-yv12+
   #:+pixelformat-iyuv+
   #:+pixelformat-yuy2+
   #:+pixelformat-uyvy+
   #:+pixelformat-yvyu+
   #:+pixelformat-nv12+
   #:+pixelformat-anv21+
   #:+pixelformat-p010+
   #:+pixelformat-external-oes+
   #:+pixelformat-mjpg+
   #:+pixelformat-rgba32+
   #:+pixelformat-argb32+
   #:+pixelformat-bgra32+
   #:+pixelformat-abgr32+
   #:+pixelformat-xrgb32+
   #:+pixelformat-bgrx32+
   #:+pixelformat-xbgr32+

   ;; texture properties
   #:+prop-texture-colorspace-number+
   #:+prop-texture-format-number+
   #:+prop-texture-access-number+
   #:+prop-texture-width-number+
   #:+prop-texture-height-number+
   #:+prop-texture-sdr-white-point-float+
   #:+prop-texture-hdr-headroom-float+
   #:+prop-texture-d3d11-texture-pointer+
   #:+prop-texture-d3d11-texture-u-pointer+
   #:+prop-texture-d3d11-texture-v-pointer+
   #:+prop-texture-d3d12-texture-pointer+
   #:+prop-texture-d3d12-texture-u-pointer+
   #:+prop-texture-d3d12-texture-v-pointer+
   #:+prop-texture-opengl-texture-number+
   #:+prop-texture-opengl-texture-uv-number+
   #:+prop-texture-opengl-texture-u-number+
   #:+prop-texture-opengl-texture-v-number+
   #:+prop-texture-opengl-texture-target-number+
   #:+prop-texture-opengl-tex-w-float+
   #:+prop-texture-opengl-tex-h-float+
   #:+prop-texture-opengles2-texture-number+
   #:+prop-texture-opengles2-texture-uv-number+
   #:+prop-texture-opengles2-texture-u-number+
   #:+prop-texture-opengles2-texture-v-number+
   #:+prop-texture-opengles2-texture-target-number+
   #:+prop-texture-vulkan-texture-number+
   #:+textureaccess-static+
   #:+textureaccess-streaming+
   #:+textureaccess-target+

   ;; texture create properties
   #:+prop-texture-create-colorspace-number+
   #:+prop-texture-create-format-number+
   #:+prop-texture-create-access-number+
   #:+prop-texture-create-width-number+
   #:+prop-texture-create-height-number+
   #:+prop-texture-create-sdr-white-point-float+
   #:+prop-texture-create-hdr-headroom-float+
   #:+prop-texture-create-d3d11-texture-pointer+
   #:+prop-texture-create-d3d11-texture-u-pointer+
   #:+prop-texture-create-d3d11-texture-v-pointer+
   #:+prop-texture-create-d3d12-texture-pointer+
   #:+prop-texture-create-d3d12-texture-u-pointer+
   #:+prop-texture-create-d3d12-texture-v-pointer+
   #:+prop-texture-create-metal-pixelbuffer-pointer+
   #:+prop-texture-create-opengl-texture-number+
   #:+prop-texture-create-opengl-texture-uv-number+
   #:+prop-texture-create-opengl-texture-u-number+
   #:+prop-texture-create-opengl-texture-v-number+
   #:+prop-texture-create-opengles2-texture-number+
   #:+prop-texture-create-opengles2-texture-uv-number+
   #:+prop-texture-create-opengles2-texture-u-number+
   #:+prop-texture-create-opengles2-texture-v-number+
   #:+prop-texture-create-vulkan-texture-number+

   #:+prop-gamepad-cap-mono-led-boolean+
   #:+prop-gamepad-cap-rgb-led-boolean+
   #:+prop-gamepad-cap-player-led-boolean+
   #:+prop-gamepad-cap-rumble-boolean+
   #:+prop-gamepad-cap-trigger-rumble-boolean+

   #:+gamepad-axis-invalid+
   #:+gamepad-axis-leftx+
   #:+gamepad-axis-lefty+
   #:+gamepad-axis-rightx+
   #:+gamepad-axis-righty+
   #:+gamepad-axis-left-trigger+
   #:+gamepad-axis-right-trigger+
   #:+gamepad-axis-count+

   #:+gamepad-axis-invalid+
   #:+gamepad-axis-leftx+
   #:+gamepad-axis-lefty+
   #:+gamepad-axis-rightx+
   #:+gamepad-axis-righty+
   #:+gamepad-axis-left-trigger+
   #:+gamepad-axis-right-trigger+
   #:+gamepad-axis-count+

   #:+gamepad-button-invalid+
   #:+gamepad-button-south+
   #:+gamepad-button-east+
   #:+gamepad-button-north+
   #:+gamepad-button-back+
   #:+gamepad-button-guide+
   #:+gamepad-button-start+
   #:+gamepad-button-left-stick+
   #:+gamepad-button-right-stick+
   #:+gamepad-button-left-shoulder+
   #:+gamepad-button-right-shoulder+
   #:+gamepad-button-dpad-up+
   #:+gamepad-button-dpad-down+
   #:+gamepad-button-dpad-left+
   #:+gamepad-button-dpad-right+
   #:+gamepad-button-misc1+
   #:+gamepad-button-right-paddle1+
   #:+gamepad-button-left-paddle1+
   #:+gamepad-button-right-paddle2+
   #:+gamepad-button-left-paddle2+
   #:+gamepad-button-touchpad+
   #:+gamepad-button-misc2+
   #:+gamepad-button-misc3+
   #:+gamepad-button-misc4+
   #:+gamepad-button-misc5+
   #:+gamepad-button-misc6+
   #:+gamepad-button-count+

   #:+gamepad-button-label-unknown+
   #:+gamepad-button-label-a+
   #:+gamepad-button-label-b+
   #:+gamepad-button-label-x+
   #:+gamepad-button-label-y+
   #:+gamepad-button-label-cross+
   #:+gamepad-button-label-circle+
   #:+gamepad-button-label-square+
   #:+gamepad-button-label-triangle+

   #:+prop-joystick-cap-mono-led-boolean+
   #:+prop-joystick-cap-rgb-led-boolean+
   #:+prop-joystick-cap-player-led-boolean+
   #:+prop-joystick-cap-rumble-boolean+
   #:+prop-joystick-cap-trigger-rumble-boolean+

   #:+prop-renderer-name-string+
   #:+prop-renderer-window-pointer+
   #:+prop-renderer-surface-pointer+
   #:+prop-renderer-vsync-number+
   #:+prop-renderer-max-texture-size-number+
   #:+prop-renderer-texture-formats-pointer+
   #:+prop-renderer-output-colorspace-number+
   #:+prop-renderer-hdr-enabled-boolean+
   #:+prop-renderer-sdr-white-point-float+
   #:+prop-renderer-hdr-headroom-float+
   #:+prop-renderer-d3d9-device-pointer+
   #:+prop-renderer-d3d11-device-pointer+
   #:+prop-renderer-d3d11-swapchain-pointer+
   #:+prop-renderer-d3d12-device-pointer+
   #:+prop-renderer-d3d12-swapchain-pointer+
   #:+prop-renderer-d3d12-command-queue-pointer+
   #:+prop-renderer-vulkan-instance-pointer+
   #:+prop-renderer-vulkan-surface-number+
   #:+prop-renderer-vulkan-physical-device-pointer+
   #:+prop-renderer-vulkan-device-pointer+
   #:+prop-renderer-vulkan-graphics-queue-family-index-number+
   #:+prop-renderer-vulkan-present-queue-family-index-number+
   #:+prop-renderer-vulkan-swapchain-image-count-number+
   #:+prop-renderer-gpu-device-pointer+

   #:+prop-renderer-create-name-string+
   #:+prop-renderer-create-window-pointer+
   #:+prop-renderer-create-surface-pointer+
   #:+prop-renderer-create-output-colorspace-number+
   #:+prop-renderer-create-present-vsync-number+
   #:+prop-renderer-create-vulkan-instance-pointer+
   #:+prop-renderer-create-vulkan-surface-number+
   #:+prop-renderer-create-vulkan-physical-device-pointer+
   #:+prop-renderer-create-vulkan-device-pointer+
   #:+prop-renderer-create-vulkan-graphics-queue-family-index-number+
   #:+prop-renderer-create-vulkan-present-queue-family-index-number+))
