;;;; inspector.scm

;;;; This example illustrates the geometric primitives offered by hypergiant.

;;;; NOTE:
;;;; If this file is compiled, since it uses glls-render, it must also be linked with OpenGL
;;;; E.g.:
;;;; csc -lGL inspector.scm

;;;; Use arrow keys (and shift) to rotate, zoom camera. Numbers 1-6 toggle different meshes.

(import chicken scheme)
(use hypergiant)

(define scene (make-parameter #f))
(define camera (make-parameter #f))
(define pan (make-parameter 0))
(define tilt (make-parameter 0))
(define zoom (make-parameter 0))
(define c-roll (make-parameter 0))
(define sphere (sphere-mesh 1 32 normals?: #t  texture-width: 1 texture-height: 1))
(define cube (cube-mesh 1 cube-map?: #t normals?: #t))
(define sky-box (cube-mesh 50 winding: #:cw cube-map?: #t))
(define rect (rectangle-mesh 2 2 texture-width: 1 texture-height: 1))
(define circle (circle-mesh 1 32 texture-radius: 0.5))
(define cylinder (cylinder-mesh 2 1 1 12 normals?: #t
                                texture-width: 1 texture-height: 1))
(define cube-sphere (sphere-mesh 1 12 type: #:cube cube-map?: #t normals?: #t))
(define earth (make-parameter #f))
(define dock (make-parameter #f))
(define active-node (make-parameter #f))
(define shiny-material (make-material 0.5 0.5 0.5 10))

(define-pipeline phong-pipeline 
  ((#:vertex input: ((position #:vec3) (normal #:vec3) (tex-coord #:vec2))
             uniform: ((mvp #:mat4) (model #:mat4) )
             output: ((p #:vec3) (n #:vec3) (t #:vec2)))
   (define (main) #:void
     (set! gl:position (* mvp (vec4 position 1.0)))
     (set! p (vec3 (* model (vec4 position 1))))
     (set! t tex-coord)
     (set! n normal)))
  ((#:fragment input: ((n #:vec3) (p #:vec3) (t #:vec2))
               use: (phong-lighting)
               uniform: ((camera-position #:vec3)
                         (inverse-transpose-model #:mat4)
                         (tex #:sampler-2d)
                         (ambient #:vec3)
                         (n-lights #:int)
                         (light-positions (#:array #:vec3 8))
                         (light-colors (#:array #:vec3 8))
                         (light-intensities (#:array #:float 8))
                         (material #:vec4))
               output: ((frag-color #:vec4)))
   (define (main) #:void
     (set! frag-color (light (texture tex t) p
                             (normalize (* (mat3 inverse-transpose-model) n)))))))

(define-pipeline cube-pipeline 
  ((#:vertex input: ((position #:vec3) (tex-coord #:vec3))
             uniform: ((mvp #:mat4))
             output: ((p #:vec3) (n #:vec3) (t #:vec3)))
   (define (main) #:void
     (set! gl:position (* mvp (vec4 position 1.0)))
     (set! t tex-coord)))
  ((#:fragment input: ((t #:vec3))
               uniform: ((tex #:sampler-cube))
               output: ((frag-color #:vec4)))
   (define (main) #:void
     (set! frag-color (texture tex t)))))

(define-pipeline cube-phong-pipeline 
  ((#:vertex input: ((position #:vec3) (normal #:vec3) (tex-coord #:vec3))
             uniform: ((mvp #:mat4) (model #:mat4) )
             output: ((p #:vec3) (n #:vec3) (t #:vec3)))
   (define (main) #:void
     (set! gl:position (* mvp (vec4 position 1.0)))
     (set! p (vec3 (* model (vec4 position 1))))
     (set! t tex-coord)
     (set! n normal)))
  ((#:fragment input: ((n #:vec3) (p #:vec3) (t #:vec3))
               use: (phong-lighting)
               uniform: ((camera-position #:vec3)
                         (inverse-transpose-model #:mat4)
                         (tex #:sampler-cube)
                         (ambient #:vec3)
                         (n-lights #:int)
                         (light-positions (#:array #:vec3 8))
                         (light-colors (#:array #:vec3 8))
                         (light-intensities (#:array #:float 8))
                         (material #:vec4))
               output: ((frag-color #:vec4)))
   (define (main) #:void
     (set! frag-color (light (texture tex t) p
                             (normalize (* (mat3 inverse-transpose-model) n)))))))

(define (add-sphere)
  (when (active-node)
    (delete-node (active-node)))
  (active-node
   (add-node (scene) phong-pipeline-render-pipeline
             mesh: sphere
             tex: (earth)
             material: shiny-material)))

(define (add-cube-sphere)
  (when (active-node)
    (delete-node (active-node)))
  (active-node
   (add-node (scene) cube-phong-pipeline-render-pipeline
             mesh: cube-sphere
             tex: (dock)
             material: shiny-material)))

(define (add-cube)
  (when (active-node)
    (delete-node (active-node)))
  (active-node
   (add-node (scene) cube-phong-pipeline-render-pipeline
             mesh: cube
             tex: (dock)
             material: shiny-material)))

(define (add-cylinder)
  (when (active-node)
    (delete-node (active-node)))
  (active-node
   (add-node (scene) phong-pipeline-render-pipeline
             mesh: cylinder
             tex: (earth)
             material: shiny-material)))

(define (add-rect)
  (when (active-node)
    (delete-node (active-node)))
  (active-node
   (add-node (scene) texture-pipeline-render-pipeline
             mesh: rect
             tex: (earth))))

(define (add-circle)
  (when (active-node)
    (delete-node (active-node)))
  (active-node
   (add-node (scene) texture-pipeline-render-pipeline
             mesh: circle
             tex: (earth))))

(define keys (make-bindings
              `((quit ,+key-escape+ press: ,stop)
                (tilt-up ,+key-up+ toggle: ,tilt)
                (tilt-down ,+key-down+ reverse-toggle: ,tilt)
                (zoom-in ,+key-up+ mods: (,+mod-shift+) reverse-toggle: ,zoom)
                (zoom-out ,+key-down+ mods: (,+mod-shift+) toggle: ,zoom)
                (pan-right ,+key-right+ toggle: ,pan)
                (pan-left ,+key-left+ reverse-toggle: ,pan)
                (roll-right ,+key-right+ mods: (,+mod-shift+) reverse-toggle: ,c-roll)
                (roll-left ,+key-left+ mods: (,+mod-shift+) toggle: ,c-roll)
                (sphere ,+key-1+ press: ,add-sphere)
                (cube-sphere ,+key-2+ press: ,add-cube-sphere)
                (cube ,+key-3+ press: ,add-cube)
                (cylinder ,+key-4+ press: ,add-cylinder)
                (rect ,+key-5+ press: ,add-rect)
                (rect ,+key-6+ press: ,add-circle))))

(define (init)
  (gl:enable gl:+texture-cube-map-seamless+)
  (push-key-bindings keys)
  (gl:clear-color 0.9 0.9 1.0 1)
  (scene (make-scene))
  (activate-extension (scene) (lighting))
  (set-ambient-light! (scene) (make-point 0.4 0.4 0.4))
  (camera (make-camera #:perspective #:orbit (scene)))
  (camera-look-at! (camera) (make-point 0 0 0))
  (set-camera-zoom! (camera) 4)
  (let ((light (add-light (scene) (make-point 1 1 1) 100)))
    (set-node-position! light (make-point -3 1 2)))
  (dock (load-ogl-cubemap "dock/posx.jpg"
                           "dock/negx.jpg"
                           "dock/posy.jpg"
                           "dock/negy.jpg"
                           "dock/posz.jpg"
                           "dock/negz.jpg"
                           0 0 0))
  (earth (load-ogl-texture "world.png" 0 0 texture/repeats))
  (add-node (scene) cube-pipeline-render-pipeline
            mesh: sky-box
            tex: (dock))
  (add-sphere))

(define (update delta)
  (yaw-camera! (camera) (/ (pan) 30))
  (pitch-camera! (camera) (/ (tilt) 30))
  (roll-camera! (camera) (/ (c-roll) 30))
  (zoom-camera! (camera) (/ (zoom) 10)))

(start 640 480 "Inspector" resizable: #f init: init update: update)
