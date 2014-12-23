(module hypergiant-render-pipeline
  (export-pipeline
   (define-pipeline make-render-pipeline dynamic-pipeline)
   (define-alpha-pipeline make-render-pipeline dynamic-alpha-pipeline)
   dynamic-pipeline
   dynamic-alpha-pipeline
   add-node
   make-render-pipeline)

(import chicken scheme foreign)
(use (prefix hyperscene scene:) (prefix glls-render glls:) gl-utils
     miscmacros srfi-69 lolevel srfi-1 srfi-99)
(import-for-syntax (prefix glls-render glls:) (prefix hyperscene scene:) srfi-99)

(define-record-type render-pipeline
  #t #t
  (dynamic?) (shader) (scene) (scene-arrays) (make-renderable) (render-fun) (render-arrays-fun))

(define renderable-table (make-hash-table))

(define-external (dynamicRender (c-pointer renderable)) void
  ((hash-table-ref renderable-table renderable) renderable))

(define-external (dynamicPreRender (c-pointer renderable)) void
  #f)

(define-external (dynamicPostRender) void
  #f)

(define dynamic-pipeline (scene:add-pipeline #$dynamicPreRender
                                             #$dynamicRender
                                             #$dynamicPostRender
                                             #f))

(define dynamic-alpha-pipeline (scene:add-pipeline #$dynamicPreRender
                                                   #$dynamicRender
                                                   #$dynamicPostRender
                                                   #t))

(define-syntax define-pipeline
  (ir-macro-transformer
   (lambda (exp i c)
     (let* ((name (strip-syntax (cadr exp)))
            (pipeline-name (symbol-append name '-render-pipeline))
            (fast-draw-funs (symbol-append name '-fast-render-functions))
            (draw-fun (symbol-append 'render- name))
            (draw-arrays-fun (symbol-append 'render-arrays- name))
            (renderable-maker (symbol-append 'make- name '-renderable)))
       `(begin
          (glls:define-pipeline ,@(cdr exp))
          ,(if (feature? compiling:)
               `(define ,pipeline-name
                  (let-values (((_ __ ___ ____ begin render end render-arrays)
                                (,fast-draw-funs)))
                    (make-render-pipeline
                     #f ,name
                     (set-finalizer! (scene:add-pipeline begin render end #f)
                                     scene:delete-pipeline)
                     (set-finalizer! (scene:add-pipeline begin render-arrays end #f)
                                     scene:delete-pipeline)
                     ,renderable-maker
                     #f #f)))
               `(define ,pipeline-name
                  (make-render-pipeline
                   #t ,name
                   dynamic-pipeline
                   #f
                   ,renderable-maker
                   ,draw-fun
                   ,draw-arrays-fun))))))))

(define-syntax define-alpha-pipeline
  (ir-macro-transformer
   (lambda (exp i c)
     (let* ((name (strip-syntax (cadr exp)))
            (pipeline-name (symbol-append name '-render-pipeline))
            (fast-draw-funs (symbol-append name '-fast-render-functions))
            (draw-fun (symbol-append 'render- name))
            (draw-arrays-fun (symbol-append 'render-arrays- name))
            (renderable-maker (symbol-append 'make- name '-renderable)))
       `(begin
          (glls:define-pipeline ,@(cdr exp))
          ,(if (feature? compiling:)
               `(define ,pipeline-name
                  (let-values (((_ __ ___ ____ begin render end render-arrays)
                                (,fast-draw-funs)))
                    (make-render-pipeline
                     #f ,name
                     (set-finalizer! (scene:add-pipeline begin render end #t)
                                     scene:delete-pipeline)
                     (set-finalizer! (scene:add-pipeline begin render-arrays end #t)
                                     scene:delete-pipeline)
                     ,renderable-maker
                     #f #f)))
               `(define ,pipeline-name
                  (make-render-pipeline
                   #t ,name
                   dynamic-alpha-pipeline
                   #f
                   ,renderable-maker
                   ,draw-fun
                   ,draw-arrays-fun))))))))

(define-syntax export-pipeline
  (ir-macro-transformer
   (lambda (expr i c)
     (cons 'export
           (flatten
            (let loop ((pipelines (cdr expr)))
              (if (null? pipelines)
                  '()
                  (if (not (symbol? (car pipelines)))
                      (syntax-error 'export-shader "Expected a pipeline name" expr)
                      (cons (let* ((name (strip-syntax (car pipelines)))
                                   (render (symbol-append 'render- name))
                                   (make-renderable (symbol-append 'make- name
                                                                   '-renderable))
                                   (fast-funs (symbol-append name
                                                             '-fast-render-functions))
                                   (render-pipeline (symbol-append name
                                                                   '-render-pipeline))
                                   (set-base (symbol-append 'set- name '-renderable-))
                                   (set-vao (symbol-append set-base 'vao!))
                                   (set-n-elements (symbol-append set-base 'n-elements!))
                                   (set-element-type (symbol-append set-base 'element-type!))
                                   (set-mode (symbol-append set-base 'mode!))
                                   (set-offset (symbol-append set-base 'offset!)))
                              (list name render make-renderable fast-funs
                                    set-vao set-n-elements set-element-type
                                    set-mode set-offset render-pipeline))
                            (loop (cdr pipelines)))))))))))

(define (add-node parent pipeline . args)
  (let ((node (if (render-pipeline? pipeline)
                 (apply add-node* parent pipeline args)
                 (apply scene:add-node parent pipeline args))))
    (if* (get-keyword position: args)
         (scene:set-node-position! node it))
    (if* (get-keyword radius: args)
         (scene:set-node-bounding-sphere! node it))
    node))

(define (add-node* parent pipeline . args)
  (define current-vars (list mvp: (scene:current-camera-model-view-projection)
                             view: (scene:current-camera-view)
                             projection: (scene:current-camera-projection)
                             view-projection: (scene:current-camera-view-projection)
                             camera-position: (scene:current-camera-position)
                             inverse-transpose-model: (scene:current-inverse-transpose-model)
                             n-lights: (scene:n-current-lights)
                             light-positions: (scene:current-light-positions)
                             light-colors: (scene:current-light-colors)
                             light-intensities: (scene:current-light-intensities)
                             light-directions: (scene:current-light-directions)
                             ambient: (scene:current-ambient-light)))
  (let* ((glls-pipeline (render-pipeline-shader pipeline))
         (make-renderable (render-pipeline-make-renderable pipeline))
         (data (allocate (glls:renderable-size glls-pipeline)))
         (mesh (get-keyword mesh: args))
         (usage (get-keyword usage: args (lambda () #:static)))
         (draw-arrays? (or (get-keyword draw-arrays?: args)
                           (not (and mesh (mesh-index-type mesh)))))
         (dynamic? (render-pipeline-dynamic? pipeline))
         (hps-pipeline (if (or dynamic? (not draw-arrays?))
                           (render-pipeline-scene pipeline)
                           (render-pipeline-scene-arrays pipeline)))
         (node (scene:add-node parent
                               data 
                               hps-pipeline
                               (foreign-value "&free" c-pointer))))
    (when mesh
      (unless (mesh-vao mesh)
        (mesh-make-vao! mesh (glls:pipeline-mesh-attributes glls-pipeline) usage)))
    (apply make-renderable data: data (append args
                                              (list model: (scene:node-transform node))
                                              current-vars))
    (when dynamic?
      (hash-table-set! renderable-table data
                       (if draw-arrays?
                           (render-pipeline-render-arrays-fun pipeline)
                           (render-pipeline-render-fun pipeline))))
    node))

) ; end module hypergiant-render-pipeline
