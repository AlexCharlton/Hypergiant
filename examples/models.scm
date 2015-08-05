;;;; models.scm

;;;; This example illustrates IQM model loading

;;;; NOTE:
;;;; If this file is compiled, since it uses glls-render, it must also be linked with OpenGL
;;;; E.g.:
;;;; csc -lGL models.scm

;;;; Use arrow keys (and shift) to rotate, zoom camera.

(import chicken scheme)
(use hypergiant lolevel)

(define scene (make-parameter #f))
(define camera (make-parameter #f))
(define pan (make-parameter 0))
(define tilt (make-parameter 0))
(define zoom (make-parameter 0))
(define c-roll (make-parameter 0))

;; (define iqm-frankie (load-iqm "Frankie.iqm"))
;; (define frankie-run (load-iqm "Frankie_Run.iqm"))
(define iqm-model (load-iqm "mrfixit.iqm"))

(define model (iqm->mesh iqm-model '(position tex-coord blend-indexes blend-weights)))

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
             output: ((tex-c #:vec2)))
   (define (main) #:void
     (let* ((bindices #:vec4 blend-indexes)
            (bweights #:vec4 blend-weights)
            (m #:mat4 (* (array-ref bone-matrices (int bindices.x))
                         bweights.x)))
       (+= m (* (array-ref bone-matrices (int bindices.y)) bweights.y))
       (+= m (* (array-ref bone-matrices (int bindices.z)) bweights.z))
       (+= m (* (array-ref bone-matrices (int bindices.w)) bweights.w))
       (set! gl:position (* mvp
                            m
                            (vec4 position 1.0)))
       (set! tex-c tex-coord))))
  ((#:fragment input: ((tex-c #:vec2))
               uniform: ((tex #:sampler-2d))
	       output: ((frag-color #:vec4)))
   (define (main) #:void
     (set! frag-color (texture tex tex-c)))))

(define (make-matrix-array n) (allocate (* 4 16 n)))
(define (add-new-animated-model parent model mesh texture base-animation)
  (let* ((n-joints (length (iqm-joints model)))
         (current-frame (make-matrix-array n-joints))
         (node (add-node parent bone-pipeline-render-pipeline
                         bone-matrices: current-frame
                         mesh: mesh
                         tex: texture))
         (animated-model (%make-animated-model node #f #f base-animation
                                               0 0.0
                                               current-frame n-joints)))
    ;;(update-animated-model! animated-model 300)
    animated-model))


(define animated-model (make-parameter #f))
(define (init)
  ;(gl:enable gl:+cull-face+)
  (push-key-bindings keys)
  (gl:clear-color 0.9 0.9 1.0 1)
  (scene (make-scene))
  (camera (make-camera #:perspective #:orbit (scene)
                       near: 0.01))
  (camera-look-at! (camera) (make-point 0 0 0))
  (set-camera-zoom! (camera) 10)
  (let* ((texture (load-ogl-texture "body.tga" 0 0 0)) ;; causing invalid enum error
         (animation (alist-ref 'idle (iqm-animations iqm-model)))
         (ani-model (add-new-animated-model (scene) iqm-model model
                                            texture animation))
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
