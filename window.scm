;;;; window.scm

;;;; Window management for Hypergiant

(module hypergiant-window
(start
 stop
 get-window-size
 get-framebuffer-size
 get-window-position
 set-window-position
 ui
 resize-hooks
 frame-rate
 get-clipboard-string
 set-clipboard-string
 make-camera)

(import chicken scheme foreign)
(use (prefix glfw3 %) (prefix glfw3-bindings %%)
     (prefix opengl-glew gl:) (prefix glls glls:)
     gl-math gl-utils (prefix hyperscene scene:)
     srfi-1 srfi-4 srfi-18 srfi-99 miscmacros)

(include "input")

(define *last-render-time* 0)
(define ui #f)
(define *ui-camera* #f)

(define (render pre-render post-render)
  (gl:clear (bitwise-ior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))
  (pre-render)
  (scene:activate-camera *ui-camera*) ; always draw UI last
  (scene:render-cameras)
  (scene:deactivate-camera *ui-camera*)
  (post-render)
  (%swap-buffers (%window)))

(define (move-ui-camera w h)
  (scene:set-camera-position! *ui-camera*
                              (make-point (/ w 2) (/ h -2) 1)))

(define resize-hooks (make-parameter (list move-ui-camera)))

(define (resize _ w h)
  (let-values (((w h) (get-framebuffer-size)))
    (gl:viewport 0 0 w h))
  (for-each (cut <> w h) (resize-hooks))
  (scene:resize-cameras w h))

(define (start* width height title . args)
  (%init)
  (%window-size-callback resize)
  (%mouse-button-callback mouse-click)
  (apply %make-window width height title
         (append (cond-expand
                   (macosx (list samples: 4
                                 context-version-major: 3
                                 context-version-minor: 2
                                 opengl-forward-compat: #t
                                 opengl-profile: %+opengl-core-profile+))
                   (gles '())
                   (else (list samples: 4
                               context-version-major: 3
                               context-version-minor: 3)))
                 args))
  (%%set-cursor-pos-callback (%window) #$hpgCursorPositionCallback)
  (%%set-scroll-callback (%window) #$hpgScrollCallback)
  (%%set-key-callback (%window) #$hpgKeyCallback)
  (%%set-char-callback (%window) #$hpgCharCallback)
  (gl:init)
  (gl:enable gl:+depth-test+)
  (gl:enable gl:+blend+)
  (gl:blend-func gl:+src-alpha+ gl:+one-minus-src-alpha+)
  (cond-expand
    ((not gles) (gl:enable gl:+multisample+))
    (else))

  (set! ui (scene:make-scene))
  (set! *ui-camera* (scene:make-camera #:ortho #:position ui))
  (scene:deactivate-camera *ui-camera*)
  (resize #f width height)
  (gl:get-error) ;; silence error caused by GLEW
  (glls:compile-pipelines)
  ((get-keyword init: args (lambda () (lambda () #f))))
  (gc)
  (set! *last-render-time* (%get-time))
  (let ((update (get-keyword update: args (lambda () (lambda (delta) #f))))
        (pre-render (get-keyword pre-render: args (lambda () (lambda () #f))))
        (post-render (get-keyword post-render: args (lambda () (lambda () #f)))))
    (let loop ()
      (let* ((time (%get-time))
             (delta (- time *last-render-time*)))
        (add-frame-time time)
        (update delta)
        (scene:update-scenes)
        (scene:activate-camera *ui-camera*)
        (scene:update-cameras)
        (scene:deactivate-camera *ui-camera*)
        (render pre-render post-render)
        (check-error)
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

(define (get-framebuffer-size)
  (%get-framebuffer-size (%window)))

(define (get-window-position)
  (%get-window-position (%window)))

(define (set-window-position x y)
  (%set-window-position (%window) x y))

(define (get-clipboard-string)
  (%get-clipboard-string (%window)))

(define (set-clipboard-string string)
  (%set-clipboard-string (%window) string))

(define (make-camera type style scene #!key (near 1) (far 10000) (angle 70)
                     width height
                     (viewport-width-ratio 1.0) (viewport-height-ratio 1.0)
                     static-viewport?)
  (when (or (not width) (not height))
    (receive (w h) (get-window-size)
      (set! width w)
      (set! height h)))
  (scene:make-camera type style scene near: near far: far angle: angle
                     width: width height: height
                     viewport-width-ratio: viewport-width-ratio
                     viewport-height-ratio: viewport-height-ratio
                     static-viewport?: static-viewport?))

;; Frame rate calculation
;; We use a circular buffer to track subsequent frame times
;; Frame rate is calculated averaging the total time between the most recent frame and oldest frame
(define n-frames-1 20) ; number of frame times we keep, minus one, because that's more useful
(define frame-times (make-list (add1 n-frames-1) 0)) ; circular buffer 
(define frame-times-counter 0) ; where are we in the frame-times list?

;; Call every frame
(define (add-frame-time time)
  (set! frame-times-counter
    (if (= frame-times-counter n-frames-1)
        0
        (add1 frame-times-counter)))
  (set! (list-ref frame-times frame-times-counter) time))

(define (frame-rate)
  (let ((oldest-frame (list-ref frame-times frame-times-counter))
        (newest-frame (list-ref frame-times (if (= frame-times-counter n-frames-1)
                                               0
                                               (add1 frame-times-counter)))))
    (/ n-frames-1 (- oldest-frame newest-frame))))

) ; end module hypergiant-window
