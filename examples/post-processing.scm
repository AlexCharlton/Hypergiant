;;;; post-processing.scm

;;;; This example illustrates how post-processing effects can be applied in Hypergiant, implementing a single-axis Gaussian blur

;;;; NOTE:
;;;; If this file is compiled, since it uses glls-render, it must also be linked with OpenGL
;;;; E.g.:
;;;; csc -lGL post-processing.scm


(import chicken scheme)
(use hypergiant)

(define scene (make-parameter #f))
(define camera (make-parameter #f))
(define framebuffer (make-parameter #f))
(define framebuffer-texture (make-parameter #f))
(define renderbuffer (make-parameter #f))
(define width 400)
(define height 400)

(define square (rectangle-mesh 1 1 color: (lambda (i)
                                            (list-ref '((1 0 0)
                                                        (0 1 0)
                                                        (0 0 1)
                                                        (1 0 1))
                                                      i))))

(define canvas (rectangle-mesh 2 2))
(define canvas-renderable (make-parameter #f))

(define-pipeline blur-x-pipeline
  ((#:vertex input: ((position #:vec3))
             output: ((tex-c #:vec2))) 
   (define (main) #:void
     (set! tex-c (+ (* 0.5 position.xy) 0.5))
     (set! gl:position (vec4 position 1))))
  ((#:fragment input: ((tex-c #:vec2))
               uniform: ((tex #:sampler-2d))
	       output: ((frag-color #:vec4)))
   (define weights (#:array float 9)
     #(0.05 0.09 0.12 0.15 0.16 0.15 0.12 0.09 0.05))
   (define blur-size (const float) (/ 1.0 256.0))
   (define (main) #:void
     (let ((sum #:vec4 (vec4 0)))
       (do-times (i 9)
         (+= sum (* (texture tex (vec2 (+ tex-c.x
                                          (* (- i 4.0) blur-size))
                                       tex-c.y))
                    (vector-ref weights i))))
       (set! frag-color sum)))))

(define (init)
  (scene (make-scene))
  (camera (make-camera #:perspective #:orbit (scene)))
  (add-node (scene) color-pipeline-render-pipeline
            mesh: square)
  (deactivate-camera (camera))

  (receive (fbo tex r) (gl:create-framebuffer width height)
    (framebuffer fbo)
    (framebuffer-texture tex)
    (renderbuffer r))

  (mesh-make-vao! canvas (pipeline-mesh-attributes blur-x-pipeline))
  (canvas-renderable (make-blur-x-pipeline-renderable
                      mesh: canvas
                      tex: (framebuffer-texture))))

(define (update delta)
  (update-camera (camera)))

(define (pre-render)
  (gl:with-framebuffer (framebuffer)
    (gl:clear (bitwise-ior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))
    (render-camera (camera)))
  (render-blur-x-pipeline (canvas-renderable)))

(start width height "Post-processing" init: init pre-render: pre-render
       update: update resizable: #f)
