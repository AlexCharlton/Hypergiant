(define-syntax define-pipeline
  (ir-macro-transformer
   (lambda (exp i c)
     (let* ((name (strip-syntax (cadr exp)))
            (pipeline-name (symbol-append name '-render-pipeline))
            (fast-draw-funs (symbol-append name '-fast-render-functions))
            (renderable-maker (symbol-append 'make- name '-renderable)))
       ;; TODO durring interpretation:
       ;; Add a pipeline that accesses a hash of renderable -> render-func
       `(begin
          (glls:define-pipeline ,@(cdr exp))
          (define ,pipeline-name
            (let-values (((_ __ __ begin render end) (,fast-draw-funs)))
              (list ,name
                    (set-finalizer! (scene:add-pipeline begin render end #f)
                                    scene:delete-pipeline)
                    ,renderable-maker))))))))

(define-syntax export-pipeline
  (ir-macro-transformer
   (lambda (expr i c)
     (if (and (not (= (length expr) 2))
            (symbol? (cadr expr)))
         (syntax-error 'export-shader "Expected a pipeline name" expr))
     (let* ((name (strip-syntax (cadr expr)))
            (pipeline (symbol-append name '-render-pipeline)))
       `(begin
          (glls:export-pipeline ,name)
          (export ,pipeline))))))

(export-pipeline mesh-shader)
(export-pipeline color-shader)

(define-pipeline mesh-shader
  ((#:vertex) ((vertex #:vec3) #:uniform (mvp #:mat4))
   (define (main) #:void
     (set! gl:position (* mvp (vec4 vertex 1.0))))
   -> ())
  ((#:fragment) (#:uniform (color #:vec3))
   (define (main) #:void
     (set! frag-color (vec4 color 1.0)))
   -> ((frag-color #:vec4))))

(define-pipeline color-shader
  ((#:vertex) ((vertex #:vec3) (color #:vec3) #:uniform (mvp #:mat4))
   (define (main) #:void
     (set! gl:position (* mvp (vec4 vertex 1.0)))
     (set! c color))
   -> ((c #:vec3)))
  ((#:fragment) ((c #:vec3))
   (define (main) #:void
     (set! frag-color (vec4 c 1.0)))
   -> ((frag-color #:vec4))))

