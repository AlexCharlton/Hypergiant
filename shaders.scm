;;;; shaders.scm

;;;; glls shaders and pipelines for common mesh types

(module hypergiant-shaders
(phong-lighting
 calc-bone-matrix
 set-max-lights!
 color-conversion)

(import chicken scheme)
(use hypergiant-render-pipeline
     (prefix glls-render glls:) (prefix hyperscene scene:))
(import-for-syntax glls-compiler)

(export-pipeline
 mesh-pipeline
 color-pipeline
 texture-pipeline
 sprite-pipeline
 text-pipeline)

(glls:define-shader calc-bone-matrix
    (#:vertex uniform: ((bone-matrices (#:array #:mat4 100))) ;; TODO solve magic number
                export: (calc-bone-matrix))
    (define (calc-bone-matrix (bindices #:vec4) (bweights #:vec4))
          #:mat4
      (let ((m #:mat4 (* (array-ref bone-matrices (int bindices.x))
                         bweights.x)))
        (+= m (* (array-ref bone-matrices (int bindices.y)) bweights.y))
        (+= m (* (array-ref bone-matrices (int bindices.z)) bweights.z))
        (+= m (* (array-ref bone-matrices (int bindices.w)) bweights.w))
        m)))

(cond-expand
  (gles (include "es-pipelines"))
  (else (include "pipelines")))

;; Via: http://www.chilliant.com/rgb2hsv.html
(glls:define-shader color-conversion
  (#:fragment export: (hue->rgb
                       hsv->rgb
                       hsl->rgb
                       rgb->hsl
                       rgb->hsv))

  (define epsilon #:float 0.0000000001)

  (define (hue->rgb (h #:float)) #:vec3
    (let ((r #:float (- (abs (- (* h 6) 3)) 1))
          (g #:float (- 2 (abs (- (* h 6) 2))))
          (b #:float (- 2 (abs (- (* h 6) 4)))))
      (clamp (vec3 r g b) 0 1)))

  (define (hsv->rgb (hsv #:vec3)) #:vec3
    (* (+ (* (- (hue->rgb hsv.x)
                1)
             hsv.y)
          1)
       hsv.z))

  (define (hsl->rgb (hsl #:vec3)) #:vec3
    (let ((c #:float (* (- 1
                           (abs (- (* 2 hsl.z)
                                   1)))
                        hsl.y)))
      (+ (* (- (hue->rgb hsl.x)
               0.5)
            c)
         hsl.z)))

  (define (rgb->hcv (rgb #:vec3)) #:vec3
    (let* ((p #:vec4 (if (< rgb.g rgb.b)
                         (vec4 rgb.bg -1.0 2.0/3.0)
                         (vec4 rgb.gb 0.0 -1.0/3.0)))
           (q #:vec4 (if (< rgb.r p.x)
                         (vec4 p.xyw rgb.r)
                         (vec4 rgb.r p.yzx)))
           (c #:float (- q.x (min q.w q.y)))
           (h #:float (abs (+ (/ (- q.w q.y)
                                 (+ (* 6 c)
                                    epsilon))
                              q.z))))
      (vec3 h c q.x)))

  (define (rgb->hsv (rgb #:vec3)) #:vec3
    (let ((hcv #:vec3 (rgb->hcv rgb)))
      (vec3 hcv.x
            (/ hcv.y
               (+ hcv.z epsilon))
            hcv.z)))

  (define (rgb->hsl (rgb #:vec3)) #:vec3
    (let* ((hcv #:vec3 (rgb->hcv rgb))
           (l #:float (- hcv.z
                         (* hcv.y 0.5)))
           (s #:float (/ hcv.y
                         (+ (+ 1 (- (abs (- (* l 2)
                                            1)))
                               epsilon)))))
      (vec3 hcv.x s l)))
  )

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

) ; end module hypergiant-shaders
