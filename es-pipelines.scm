(define-pipeline mesh-pipeline
  ((#:vertex input: ((position #:vec3))
             uniform: ((mvp #:mat4)))
   (define (main) #:void
     (set! gl:position (* mvp (vec4 position 1.0)))))
  ((#:fragment uniform: ((color #:vec3)))
   (define (main) #:void
     (set! gl:frag-color (vec4 color 1.0)))))

(define-pipeline color-pipeline
  ((#:vertex input: ((position #:vec3) (color #:vec3))
             uniform: ((mvp #:mat4))
             output: ((c #:vec3)))
   (define (main) #:void
     (set! gl:position (* mvp (vec4 position 1.0)))
     (set! c color)))
  ((#:fragment input: ((c #:vec3)))
   (define (main) #:void
     (set! gl:frag-color (vec4 c 1.0)))))

(define-pipeline texture-pipeline
  ((#:vertex input: ((position #:vec3) (tex-coord #:vec2))
             uniform: ((mvp #:mat4))
             output: ((tex-c #:vec2)))
   (define (main) #:void
     (set! gl:position (* mvp (vec4 position 1.0)))
     (set! tex-c tex-coord)))
  ((#:fragment input: ((tex-c #:vec2))
               uniform: ((tex #:sampler-2d)))
   (define (main) #:void
     (set! gl:frag-color (texture-2d tex tex-c)))))

(define-alpha-pipeline sprite-pipeline
  ((#:vertex input: ((position #:vec3) (tex-coord #:vec2))
             uniform: ((mvp #:mat4))
             output: ((tex-c #:vec2)))
   (define (main) #:void
     (set! gl:position (* mvp (vec4 position 1.0)))
     (set! tex-c tex-coord)))
  ((#:fragment input: ((tex-c #:vec2))
               uniform: ((tex #:sampler-2d)))
   (define (main) #:void
     (set! gl:frag-color (texture-2d tex tex-c)))))

(define-pipeline text-pipeline
  ((#:vertex input: ((position #:vec2) (tex-coord #:vec2))
             uniform: ((mvp #:mat4))
             output: ((tex-c #:vec2))) 
   (define (main) #:void
     (set! gl:position (* mvp (vec4 position 0.0 1.0)))
     (set! tex-c tex-coord)))
  ((#:fragment input: ((tex-c #:vec2))
               uniform: ((tex #:sampler-2d)
                         (color #:vec3)))
   (define (main) #:void
     (let ((r #:float (field (texture-2d tex tex-c) r)))
       (set! gl:frag-color (vec4 color r))))))
