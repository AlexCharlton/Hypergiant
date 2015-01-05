(module hypergiant ()

(import chicken scheme foreign)
(use (prefix glfw3 %) (prefix glfw3-bindings %%) (prefix glls-render glls:)
     (prefix opengl-glew gl:) gl-math
     gl-utils (prefix hyperscene scene:) gl-type
     srfi-1 srfi-18 srfi-42 srfi-99 data-structures
     random-mtzig miscmacros noise soil
     hypergiant-render-pipeline)

(include "utils")
(include "input")
(include "math")
(include "geometry")
(include "shaders")
(include "sprites")
(include "window")

(reexport (prefix opengl-glew gl:)
          (except hyperscene
                  init
                  resize-cameras
                  add-node
                  set-max-lights!)
          (except glls-render
                  define-pipeline
                  export-pipeline)
          hypergiant-render-pipeline
          gl-math
          (prefix gl-utils-core gl:)
          gl-utils-ply
          gl-utils-srfi-4
          gl-utils-bytevector
          gl-utils-mesh
          gl-type
          noise
          soil
          (only glfw3
                get-time
                +press+ +release+ +repeat+
                +joystick-last+
                +joystick-16+
                +joystick-15+
                +joystick-14+
                +joystick-13+
                +joystick-12+
                +joystick-11+
                +joystick-10+
                +joystick-9+
                +joystick-8+
                +joystick-7+
                +joystick-6+
                +joystick-5+
                +joystick-4+
                +joystick-3+
                +joystick-2+
                +joystick-1+
                +mouse-button-middle+
                +mouse-button-right+
                +mouse-button-left+
                +mouse-button-last+
                +mouse-button-8+
                +mouse-button-7+
                +mouse-button-6+
                +mouse-button-5+
                +mouse-button-4+
                +mouse-button-3+
                +mouse-button-2+
                +mouse-button-1+
                +mod-super+
                +mod-alt+
                +mod-control+
                +mod-shift+
                +key-last+
                +key-menu+
                +key-right-super+
                +key-right-alt+
                +key-right-control+
                +key-right-shift+
                +key-left-super+
                +key-left-alt+
                +key-left-control+
                +key-left-shift+
                +key-kp-equal+
                +key-kp-enter+
                +key-kp-add+
                +key-kp-subtract+
                +key-kp-multiply+
                +key-kp-divide+
                +key-kp-decimal+
                +key-kp-9+
                +key-kp-8+
                +key-kp-7+
                +key-kp-6+
                +key-kp-5+
                +key-kp-4+
                +key-kp-3+
                +key-kp-2+
                +key-kp-1+
                +key-kp-0+
                +key-f25+
                +key-f24+
                +key-f23+
                +key-f22+
                +key-f21+
                +key-f20+
                +key-f19+
                +key-f18+
                +key-f17+
                +key-f16+
                +key-f15+
                +key-f14+
                +key-f13+
                +key-f12+
                +key-f11+
                +key-f10+
                +key-f9+
                +key-f8+
                +key-f7+
                +key-f6+
                +key-f5+
                +key-f4+
                +key-f3+
                +key-f2+
                +key-f1+
                +key-pause+
                +key-print-screen+
                +key-num-lock+
                +key-scroll-lock+
                +key-caps-lock+
                +key-end+
                +key-home+
                +key-page-down+
                +key-page-up+
                +key-up+
                +key-down+
                +key-left+
                +key-right+
                +key-delete+
                +key-insert+
                +key-backspace+
                +key-tab+
                +key-enter+
                +key-escape+
                +key-world-2+
                +key-world-1+
                +key-grave-accent+
                +key-right-bracket+
                +key-backslash+
                +key-left-bracket+
                +key-z+
                +key-y+
                +key-x+
                +key-w+
                +key-v+
                +key-u+
                +key-t+
                +key-s+
                +key-r+
                +key-q+
                +key-p+
                +key-o+
                +key-n+
                +key-m+
                +key-l+
                +key-k+
                +key-j+
                +key-i+
                +key-h+
                +key-g+
                +key-f+
                +key-e+
                +key-d+
                +key-c+
                +key-b+
                +key-a+
                +key-equal+
                +key-semicolon+
                +key-9+
                +key-8+
                +key-7+
                +key-6+
                +key-5+
                +key-4+
                +key-3+
                +key-2+
                +key-1+
                +key-0+
                +key-slash+
                +key-period+
                +key-minus+
                +key-comma+
                +key-apostrophe+
                +key-space+
                +key-unknown+))

) ; end module hypergiant
