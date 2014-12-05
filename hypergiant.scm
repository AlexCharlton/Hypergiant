(module hypergiant
  (start
   stop
   add-node
   get-window-size)

(import chicken scheme foreign)
(use (prefix glfw3 %) (prefix glfw3-bindings %%) (prefix glls-render glls:)
     (prefix opengl-glew gl:) gl-math
     gl-utils (prefix hyperscene scene:) lolevel gl-type
     srfi-1 srfi-18 srfi-42 srfi-69 srfi-99 data-structures
     random-mtzig miscmacros noise soil)
(import-for-syntax (prefix glls-render glls:) (prefix hyperscene scene:))

(include "bindings.scm")
(include "math.scm")
(include "geometry.scm")
(include "shaders.scm")

(reexport (prefix opengl-glew gl:)
          (except hyperscene
                  init
                  add-pipeline
                  delete-pipeline
                  resize-cameras
                  add-node)
          (except glls
                  define-pipeline)
          gl-math
          (prefix gl-utils-core gl:)
          gl-utils-ply
          gl-utils-srfi-4
          gl-utils-bytevector
          gl-utils-mesh
          gl-type
          noise
          soil ; TODO: wrap?
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

(define *last-render-time* 0)

(define (render)
    (%swap-buffers (%window))
    (gl:clear (bitwise-ior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))
    (if (feature? #:csi)
        (scene:render-cameras)
        (scene:unsafe-render-cameras)))

(define (resize _ w h)
  (gl:viewport 0 0 w h)
  (scene:resize-cameras))

(define (start* width height title . args)
  (%init)
  (scene:init (lambda () (%get-window-size (%window))))
  (%window-size-callback resize)
  (%mouse-button-callback mouse-click)
  (apply %make-window width height title samples: 2 args)
  (%%set-cursor-pos-callback (%window) #$hpgCursorPositionCallback)
  (%%set-scroll-callback (%window) #$hpgScrollCallback)
  (%%set-key-callback (%window) #$hpgKeyCallback)
  (%%set-char-callback (%window) #$hpgCharCallback)
  (gl:init)

  (gl:enable gl:+depth-test+)
  ;(gl:enable gl:+blend+)
  (cond-expand
    ((not gles) (gl:enable gl:+multisample+))
    (else))
  ;(gl:enable gl:+cull-face+)
  (gl:blend-func gl:+src-alpha+ gl:+one-minus-src-alpha+)

  (resize #f width height)
  ((get-keyword init: args (lambda () (lambda () #f))))
  (gc)
  (set! *last-render-time* (%get-time))
  (let ((update (get-keyword update: args (lambda () (lambda (delta) #f)))))
    (let loop ()
      (let* ((time (%get-time))
             (delta (- time *last-render-time*)))
        (update delta)
        (scene:update-scenes)
        (render)
        (gl:check-error)
        (set! *last-render-time* time)
        (when (feature? csi:)
          (thread-yield!))
        (%poll-events))
      (unless (%window-should-close? (%window))
        (loop))))
  ((get-keyword cleanup: args (lambda () (lambda () #f))))
  (gc)
  (%destroy-window (%window))
  (%terminate))

(define (start width height title . args)
  (define start** (lambda () (apply start* width height title args)))
  (if (feature? csi:)
      (thread-start! start**)
      (start**)))

(define (stop)
  (%set-window-should-close (%window) #t))

(define (get-window-size)
  (%get-window-size (%window)))

(define (get-cursor-world-position)
  (define (scale x) (sub1 (* x 2)))
  (let-values (((w h) (get-window-size))
               ((x y) (get-cursor-position)))
    (let* ((ivp (make-f32vector 16))
           (x (scale (/ x w)))
           (y (scale (- 1 (/ y h))))
           (near (make-point x y -1))
           (far (make-point x y 1)))
      (inverse (current-camera-view-projection) (gl:->pointer ivp))
      (m*vector! ivp near)
      (m*vector! ivp far)
      (values near far))))

(define (add-node pipeline parent . args)
  (let ((data (allocate (glls:renderable-size (first pipeline)))))
    (if* (get-keyword mesh: args)
         (unless (mesh-vao it)
           (mesh-make-vao! it (glls:pipeline-mesh-attributes (first pipeline))
                           (get-keyword usage: args (lambda () #:static)))))
    (apply (third pipeline) data: data
           (append args (list mvp: (current-camera-model-view-projection))))
    ;; TODO append other current-camera-*s
    (when (and (feature? csi:) (> (length pipeline) 3))
      (hash-table-set! renderable-table data (fourth pipeline)))
    (scene:add-node parent
                    data 
                    (second pipeline)
                    (foreign-value "&free" c-pointer))))

) ; end module hypergiant
