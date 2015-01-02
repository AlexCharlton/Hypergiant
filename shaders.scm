(import-for-syntax glls-compiler)

(begin-for-syntax
 (define old-glsl-version (glsl-version))
 (glsl-version 120))

(export phong-lighting
        set-max-lights!)
(export-pipeline
 mesh-pipeline
 color-pipeline
 texture-pipeline
 sprite-pipeline
 text-pipeline)

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

;; TODO light direction
(define-syntax phong-light-n
  (er-macro-transformer
   (lambda (expr r c)
     (let ((name (cadr expr))
           (n (caddr expr)))
      `(glls:define-shader ,name
          (#:fragment uniform: ((camera-position #:vec3)
                                (inverse-transpose-model #:mat4)
                                (ambient #:vec3)
                                (n-lights #:int)
                                (light-positions (#:array #:vec3 ,n))
                                (light-colors (#:array #:vec3 ,n))
                                (light-intensities (#:array #:float ,n))
                                (material #:vec4))
                      export: (light))
          (define gamma #:vec3 (vec3 (/ 1 2.2)))
          (define (light (surface-color #:vec4) (position #:vec3) (normal #:vec3))
                #:vec4
            (set! normal (* (mat3 inverse-transpose-model)
                            (normalize normal)))
            (let ((linear-color #:vec3 (* ambient surface-color.rgb)))
              (do-times (i n-lights)
                (let* ((light-position #:vec3 (array-ref light-positions i))
                       (light-color #:vec3 (array-ref light-colors i))
                       (light-intensity #:float (array-ref light-intensities i))
                       (surface-specular #:vec3 (vec3 material))
                       (specular-exponent #:float material.a)
                       (distance #:vec3 (- light-position position))
                       (intensity #:float (clamp (/ light-intensity
                                                    (pow (+ 0.001
                                                            (* distance.x distance.x)
                                                            (* distance.y distance.y)
                                                            (* distance.z distance.z))
                                                         2))
                                                 0.0 1.0))
                       (to-light #:vec3 (normalize distance))
                       (to-camera #:vec3 (normalize (- camera-position position)))
                       (diffuse-intensity #:float (max (dot normal to-light) 0.0))
                       (diffuse #:vec3 (* surface-color.rgb light-color
                                          diffuse-intensity))
                       (specular-intensity
                        #:float (if (> diffuse-intensity 0.0)
                                    (max (dot to-camera
                                              (reflect (- to-light) normal))
                                         0.0)
                                    0.0))
                       (specular #:vec3 (* light-color surface-specular
                                           (expt specular-intensity
                                                 specular-exponent))))
                  (+= linear-color (* intensity (+ diffuse specular)))))
              (vec4 (pow linear-color gamma) surface-color.a))))))))

(phong-light-n phong-lighting-8 8)
(phong-light-n phong-lighting-16 16)
(phong-light-n phong-lighting-32 32)
(phong-light-n phong-lighting-64 64)

(define phong-lighting phong-lighting-8)

(define (set-max-lights! n)
  (scene:set-max-lights! n)
  (set! phong-lighting (cond
                        ((<= n 8) phong-lighting-8)
                        ((<= n 16) phong-lighting-16)
                        ((<= n 32) phong-lighting-32)
                        ((<= n 64) phong-lighting-64)
                        (else (error 'set-max-lights! "Too many lights" n)))))

(begin-for-syntax (glsl-version old-glsl-version))
