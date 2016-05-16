;;;; pipelines.scm

;;;; Standard GL pipelines, conditionally imported by shaders.scm

(export-pipeline normal-pipeline)

(define-pipeline mesh-pipeline
  ((#:vertex input: ((position #:vec3))
             uniform: ((mvp #:mat4)))
   (define (main) #:void
     (set! gl:position (* mvp (vec4 position 1.0)))))
  ((#:fragment uniform: ((color #:vec3))
	       output: ((frag-color #:vec4)))
   (define (main) #:void
     (set! frag-color (vec4 color 1.0)))))

(define-pipeline color-pipeline
  ((#:vertex input: ((position #:vec3) (color #:vec3))
             uniform: ((mvp #:mat4))
             output: ((c #:vec3)))
   (define (main) #:void
     (set! gl:position (* mvp (vec4 position 1.0)))
     (set! c color)))
  ((#:fragment input: ((c #:vec3))
	       output: ((frag-color #:vec4)))
   (define (main) #:void
     (set! frag-color (vec4 c 1.0)))))

(define-pipeline texture-pipeline
  ((#:vertex input: ((position #:vec3) (tex-coord #:vec2))
             uniform: ((mvp #:mat4))
             output: ((tex-c #:vec2)))
   (define (main) #:void
     (set! gl:position (* mvp (vec4 position 1.0)))
     (set! tex-c tex-coord)))
  ((#:fragment input: ((tex-c #:vec2))
               uniform: ((tex #:sampler-2d))
	       output: ((frag-color #:vec4)))
   (define (main) #:void
     (set! frag-color (texture tex tex-c)))))

(define-alpha-pipeline sprite-pipeline
  ((#:vertex input: ((position #:vec3) (tex-coord #:vec2))
             uniform: ((mvp #:mat4))
             output: ((tex-c #:vec2)))
   (define (main) #:void
     (set! gl:position (* mvp (vec4 position 1.0)))
     (set! tex-c tex-coord)))
  ((#:fragment input: ((tex-c #:vec2))
               uniform: ((tex #:sampler-2d))
	       output: ((frag-color #:vec4)))
   (define (main) #:void
     (set! frag-color (texture tex tex-c)))))

(define-alpha-pipeline text-pipeline
  ((#:vertex input: ((position #:vec2) (tex-coord #:vec2))
             uniform: ((mvp #:mat4))
             output: ((tex-c #:vec2))) 
   (define (main) #:void
     (set! gl:position (* mvp (vec4 position 0.0 1.0)))
     (set! tex-c tex-coord)))
  ((#:fragment input: ((tex-c #:vec2))
               uniform: ((tex #:sampler-2d)
                         (color #:vec3))
	       output: ((frag-color #:vec4)))
   (define (main) #:void
     (let ((r #:float (field (texture tex tex-c) r)))
       (set! frag-color (vec4 color r))))))

(define-pipeline normal-pipeline
  ((#:vertex input: ((position #:vec3) (normal #:vec3))
             output: ((vnormal #:vec3)))
   (define (main) #:void
     (set! gl:position (vec4 position 1.0))
     (set! vnormal normal)))
  ((#:geometry input: ((vnormal (#:array vec3)))
               uniform: ((mvp #:mat4))
               output: ((color #:vec3))
               prelude: "layout(points) in;\n\
layout(line_strip, max_vertices = 2) out;\n")
   (define (main) #:void
     (let ((n #:vec3 (array-ref vnormal 0))
           (v0 #:vec4 (field (array-ref gl_in 0) gl:Position))
           (v1 #:vec4 (+ v0 (vec4 (normalize n) 0))))
       (set! color (abs n))
       (set! gl:Position (* mvp v0))
       (emit-vertex)
       (set! gl:Position (* mvp v1))
       (emit-vertex)
       (end-primitive))))
  ((#:fragment input: ((color #:vec3))
               output: ((frag-color #:vec4)))
   (define (main) #:void
     (set! frag-color (vec4 color 1)))))

