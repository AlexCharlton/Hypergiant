;;;; alpha.scm

;;;; This example illustrates alpha sorting.

;;;; NOTE:
;;;; If this file is compiled, since it uses glls-render, it must also be linked with OpenGL
;;;; E.g.:
;;;; csc -lGL alpha.scm

;;;; Use arrow keys (and shift) to rotate, zoom camera.

(import chicken scheme)
(use hypergiant)

(define scene (make-parameter #f))
(define camera (make-parameter #f))
(define pan (make-parameter 0))
(define tilt (make-parameter 0))
(define zoom (make-parameter 0))
(define c-roll (make-parameter 0))
(define cube (cube-mesh 1 color: (lambda (_) '(0.5 0.3 0.5))))
(define small-cube (mesh-copy cube))
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

(define-alpha-pipeline alpha-color-pipeline
  ((#:vertex input: ((position #:vec3) (color #:vec3))
             uniform: ((mvp #:mat4))
             output: ((c #:vec3)))
   (define (main) #:void
     (set! gl:position (* mvp (vec4 position 1.0)))
     (set! c color)))
  ((#:fragment input: ((c #:vec3))
	       output: ((frag-color #:vec4)))
   (define (main) #:void
     (set! frag-color (vec4 c 0.5)))))

(define (init)
  (gl:enable gl:+cull-face+)
  (push-key-bindings keys)
  (gl:clear-color 0.9 0.9 1.0 1)
  (scene (make-scene))
  (camera (make-camera #:perspective #:orbit (scene)))
  (camera-look-at! (camera) (make-point 0 0 0))
  (set-camera-zoom! (camera) 4)
  (add-node (scene) alpha-color-pipeline-render-pipeline
            mesh: cube)
  (add-node (scene) alpha-color-pipeline-render-pipeline
            mesh: small-cube
            position: (make-point 0 -0.01 1)
            radius: 0.4))

(define (update delta)
  (yaw-camera! (camera) (/ (pan) 30))
  (pitch-camera! (camera) (/ (tilt) 30))
  (roll-camera! (camera) (/ (c-roll) 30))
  (zoom-camera! (camera) (/ (zoom) 10)))

(start 640 480 "Alpha" resizable: #f init: init update: update)
