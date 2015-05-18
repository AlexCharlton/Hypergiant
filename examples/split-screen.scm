;;;; split-screen.scm

;;;; This example illustrates split-screen rendering.

;;;; Use arrow keys (and shift) to rotate, zoom camera.

(import chicken scheme)
(use hypergiant)

(define scene (make-parameter #f))
(define camera-r (make-parameter #f))
(define camera-l (make-parameter #f))
(define pan (make-parameter 0))
(define tilt (make-parameter 0))
(define zoom (make-parameter 0))
(define c-roll (make-parameter 0))
(define cube (cube-mesh 1 color: (lambda (_) '(0.5 0.3 0.5))))
(define small-cube (cube-mesh 1 color: (lambda (_) '(0.2 0.1 0.2))))
(mesh-transform! 'position small-cube
                 (scaling 0.4))

(define keys (make-bindings
              `((quit ,+key-escape+ press: ,stop)
                (tilt-up ,+key-up+ toggle: ,tilt)
                (tilt-down ,+key-down+ reverse-toggle: ,tilt)
                (zoom-in ,+key-up+ mods: (,+mod-shift+) reverse-toggle: ,zoom)
                (zoom-out ,+key-down+ mods: (,+mod-shift+) toggle: ,zoom)
                (pan-right ,+key-right+ toggle: ,pan)
                (pan-left ,+key-left+ reverse-toggle: ,pan)
                (roll-right ,+key-right+ mods: (,+mod-shift+) reverse-toggle: ,c-roll)
                (roll-left ,+key-left+ mods: (,+mod-shift+) toggle: ,c-roll))))

(define (init)
  (gl:enable gl:+scissor-test+) ; Enable gl:scissor

  (push-key-bindings keys)
  (gl:clear-color 0.9 0.9 1.0 1)
  (scene (make-scene))

  ;; Right camera
  (camera-r (make-camera #:perspective #:orbit (scene)
                         viewport-width-ratio: 0.5))
  (camera-look-at! (camera-r) (make-point 0 0 0))
  (set-camera-zoom! (camera-r) 4)
  (set-camera-viewport-screen-position! (camera-r) 0 1 -1 1)
  (deactivate-camera (camera-r)) ; We need manual control over rendering

  ;; Left camera
  (camera-l (make-camera #:perspective #:orbit (scene)
                         viewport-width-ratio: 0.5))
  (camera-look-at! (camera-l) (make-point 0 0 0))
  (set-camera-zoom! (camera-l) 4)
  (set-camera-viewport-screen-position! (camera-l) -1 0 -1 1)
  (deactivate-camera (camera-r)) ; We need manual control over rendering
  
  (add-node (scene) color-pipeline-render-pipeline
            mesh: cube)
  (add-node (scene) color-pipeline-render-pipeline
            mesh: small-cube
            position: (make-point 0 -0.01 1)
            radius: 0.4))

(define (update delta)
  (yaw-camera! (camera-r) (/ (pan) 30))
  (pitch-camera! (camera-r) (/ (tilt) 30))
  (roll-camera! (camera-r) (/ (c-roll) 30))
  (zoom-camera! (camera-r) (/ (zoom) 10))

  (update-camera (camera-r))
  (update-camera (camera-l)))

;; Use gl:scissor to ensure that each camera only gets rendered to one half of the screen
;; Framebuffer size is used to ensure that this works on displays of all pixel densities
(define (pre-render)
  (let-values (((w h) (get-framebuffer-size)))
    (let ((w/2 (* w 0.5)))
      (gl:scissor 0 0 w/2 h)
     (render-camera (camera-l))
     (gl:scissor w/2 0 w/2 h)
     (render-camera (camera-r))
     (gl:scissor 0 0 w h))))

(start 640 480 "Split-screen" resizable: #f init: init update: update
       pre-render: pre-render)
