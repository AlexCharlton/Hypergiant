;;;; models.scm

;;;; This example illustrates IQM model loading and animating

;;;; NOTE:
;;;; If this file is compiled, since it uses glls-render, it must also be linked with OpenGL
;;;; E.g.:
;;;; csc -lGL models.scm

;;;; Use arrow keys (and shift) to rotate, zoom camera.

;;;; PS The head of this model looks messed-up since it's supposed
;;;; to have a separate texture associated with it.
;;;; This is not currently supported.
;;;; I think it looks pretty cool like this, though.

(import chicken scheme)
(use hypergiant)

(define scene (make-parameter #f))
(define camera (make-parameter #f))
(define pan (make-parameter 0))
(define tilt (make-parameter 0))
(define zoom (make-parameter 0))
(define c-roll (make-parameter 0))

(define iqm-model (load-iqm "mrfixit.iqm"))
(define animated-model (make-parameter #f))
(define model-mesh (iqm->mesh iqm-model '(position tex-coord blend-indexes blend-weights)))

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

(define-pipeline bone-pipeline
  ((#:vertex input: ((position #:vec3) (tex-coord #:vec2)
                     (blend-indexes #:vec4) (blend-weights #:vec4))
             uniform: ((mvp #:mat4)
                       (bone-matrices (#:array #:mat4 100)))
             use: (calc-bone-matrix)
             output: ((tex-c #:vec2)))
   (define (main) #:void
     (set! gl:position (* mvp
                          (calc-bone-matrix blend-indexes
                                            blend-weights)
                          (vec4 position 1.0)))
     (set! tex-c tex-coord)))
  ((#:fragment input: ((tex-c #:vec2))
               uniform: ((tex #:sampler-2d))
	       output: ((frag-color #:vec4)))
   (define (main) #:void
     (set! frag-color (texture tex tex-c)))))

(define (init)
  (gl:enable gl:+cull-face+)
  (gl:front-face gl:+cw+)
  (push-key-bindings keys)
  (gl:clear-color 0.9 0.9 1.0 1)
  (scene (make-scene))
  (camera (make-camera #:perspective #:orbit (scene)
                       near: 0.01))
  (camera-look-at! (camera) (make-point 0 0 0))
  (set-camera-zoom! (camera) 10)
  (let* ((texture (load-ogl-texture "mrfixit.tga" 0 0 0)) ;; causing invalid enum error
         (animation (alist-ref 'idle (iqm-animations iqm-model)))
         (ani-model (add-new-animated-model (scene)
                                            bone-pipeline-render-pipeline
                                            model: iqm-model
                                            mesh: model-mesh
                                            texture: texture
                                            base-animation: animation))
         (node (animated-sprite-node ani-model)))
    (animated-model ani-model)
    (quaternion-rotate-x (- pi/2) (node-rotation node))
    (quaternion-rotate-y (- pi/2) (node-rotation node))
    (move-node! node (make-point 0 -4 0))))

(define (update delta)
  (update-animated-model! (animated-model) delta)
  (yaw-camera! (camera) (/ (pan) 30))
  (pitch-camera! (camera) (/ (tilt) 30))
  (roll-camera! (camera) (/ (c-roll) 30))
  (zoom-camera! (camera) (/ (zoom) 10)))

(start 640 480 "Models" resizable: #f init: init update: update)
