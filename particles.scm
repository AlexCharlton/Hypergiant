;;;; particles.scm

;;;; Particle system, implemented as a Hyperscene plugin so particles can be sorted by depth before being drawn

(module hypergiant-particles
(particles
 emitter?
 emitter-node
 add-emitter
 delete-emitter
 add-particle
 delete-particle
 emitter-n-particles
 emitter-max-particles
 (for-emitter emitter-mesh mesh-update* %update-emitter %emitter-reset
              %emitter-next-particle particle-getter particle-setter
              get-vertex-attribute type->make-vector type->pointer
              emitter-emitter))

(import chicken scheme foreign)
(use (prefix glls-render glls:) (prefix opengl-glew gl:)
     gl-utils (prefix hyperscene scene:)
     srfi-1 srfi-4 srfi-99 miscmacros
     hypergiant-render-pipeline)

;;; Bindings
(foreign-declare "#include \"particles.h\"")

(define (particles)
  (foreign-value "hpgParticles" c-pointer))

(define %add-emitter
  (foreign-lambda c-pointer "hpgAddEmitter" c-pointer size_t u8vector size_t u8vector))

(define %delete-emitter
  (foreign-lambda void "hpgDeleteEmitter" c-pointer))

(define %add-particle
  (foreign-lambda c-pointer "hpgAddParticle" c-pointer))

(define %delete-particle
  (foreign-lambda void "hpgDeleteParticle" c-pointer c-pointer))

(define %emitter-reset
  (foreign-lambda void "hpgEmitterReset" c-pointer))

(define %emitter-next-particle
  (foreign-lambda c-pointer "hpgEmitterNextParticle" c-pointer))

(define %update-emitter
  (foreign-lambda void "hpgUpdateEmitter" c-pointer))

(define %emitter-n-particles
  (foreign-lambda size_t "hpgEmitterNParticles" c-pointer))

(define %emitter-max-particles
  (foreign-lambda size_t "hpgEmitterMaxParticles" c-pointer))

;;; Largely copied from gl-utils
(define (type->make-vector type)
  (ecase type
    ((char: int8: byte:) make-s8vector)
    ((uchar: uint8: unsigned-byte:) make-u8vector)
    ((short: int16:) make-s16vector)
    ((ushort: uint16: unsigned-short:) make-u16vector)
    ((int: int32: integer: integer32:) make-s32vector)
    ((uint: uint32: unsigned-int: unsigned-int32:
	    unsigned-integer: unsigned-integer32:)
     make-u32vector)
    ((float: float32:) make-f32vector)
    ((double: float64:) make-f64vector)))

(define-syntax XXX->pointer
  (ir-macro-transformer
   (lambda (e r c)
     (let* ((type (strip-syntax (cadr e)))
            (name (string->symbol (string-append (symbol->string type) "->pointer"))))
       `(define ,name
          (foreign-lambda* c-pointer ((,type v))
            "C_return(v);"))))))

(XXX->pointer u8vector)
(XXX->pointer s8vector)
(XXX->pointer u16vector)
(XXX->pointer s16vector)
(XXX->pointer u32vector)
(XXX->pointer s32vector)
(XXX->pointer f32vector)
(XXX->pointer f64vector)

(define (type->pointer type)
  (ecase type
    ((char: int8: byte:) s8vector->pointer)
    ((uchar: uint8: unsigned-byte:) u8vector->pointer)
    ((short: int16:) s16vector->pointer)
    ((ushort: uint16: unsigned-short:) u16vector->pointer)
    ((int: int32: integer: integer32:) s32vector->pointer)
    ((uint: uint32: unsigned-int: unsigned-int32:
	    unsigned-integer: unsigned-integer32:)
     u32vector->pointer)
    ((float: float32:) f32vector->pointer)
    ((double: float64:) f64vector->pointer)))

(define (get-vertex-attribute name attributes)
  (if* (find (lambda (attribute)
               (equal? name (vertex-attribute-name attribute)))
             attributes)
       it
       (error 'make-mesh "No attribute of this name in mesh's vertex-attributes" name attributes)))

;;; Scheme interface
(define-record-type emitter
  #t #t
  (emitter) (emitter-node) (node) (mesh))

;; Radius can only be set once, so choose wisely
(define (add-emitter parent pipeline . args)
  (let* ((attributes (cons '(position #:float 3)
                             (get-keyword attributes: args (lambda () '()))))
         (n-particles (get-keyword n-particles: args
                                   (lambda () (error 'add-emitter "Missing n-particles keyword: " args))))
         (mesh (make-mesh vertices: `(attributes: ,attributes
                                      n-vertices: ,n-particles)
                          indices: `(type: #:unsigned-int
                                     n-indices: ,(add1 n-particles))
                          mode: #:points))
         (node (apply add-node parent pipeline mesh: mesh usage: #:stream args))
         (emitter-node (%add-emitter node n-particles
                                     (mesh-vertex-data mesh)
                                     (mesh-stride mesh)
                                     (mesh-index-data mesh))))
    
    (if* (get-keyword radius: args)
         (scene:set-node-bounding-sphere! emitter-node it))
    (glls:set-renderable-n-elements! (scene:node-data node) 0)
    (make-emitter (scene:node-data emitter-node) emitter-node node mesh)))

(define (delete-emitter emitter)
  (scene:delete-node (emitter-node emitter))
  (scene:delete-node (emitter-emitter-node emitter)))

(define (add-particle emitter)
  (let ((new (%add-particle (emitter-emitter emitter))))
    (glls:set-renderable-n-elements! (scene:node-data (emitter-node emitter))
                                     (emitter-n-particles emitter))
    new))

(define (delete-particle emitter particle)
  (%delete-particle (emitter-emitter emitter) particle)
  (glls:set-renderable-n-elements! (scene:node-data (emitter-node emitter))
                                   (emitter-n-particles emitter)))

(define (emitter-n-particles emitter)
  (%emitter-n-particles (emitter-emitter emitter)))

(define (emitter-max-particles emitter)
  (%emitter-max-particles (emitter-emitter emitter)))

(define (particle-setter particle value offset length)
  ((foreign-lambda* void ((c-pointer to) (c-pointer from) (size_t offset)
                          (size_t length))
     "memcpy((&((char *)to)[offset]), from, length);")
   particle value offset length))

(define-for-syntax (make-setter attribute)
  `(,(symbol-append attribute '-set!)
    (let* ((attr (get-vertex-attribute ',attribute vertex-attributes))
           (type (vertex-attribute-type attr))
           (->pointer (type->pointer type))
           (offset (vertex-attribute-offset attr))
           (length (* (type->bytes type)
                      (vertex-attribute-number attr))))
      (lambda (particle value)
        (particle-setter particle (->pointer value) offset length)))))

(define (particle-getter particle return offset length)
  ((foreign-lambda* void ((c-pointer from) (c-pointer to) (size_t offset)
                          (size_t length))
     "memcpy(to, (&((char *)from)[offset]), length);")
   particle return offset length))

(define-for-syntax (make-getter attribute)
  `(,(symbol-append attribute '-ref)
    (let* ((attr (get-vertex-attribute ',attribute vertex-attributes))
           (type (vertex-attribute-type attr))
           (return ((type->make-vector type) (vertex-attribute-number attr)))
           (offset (vertex-attribute-offset attr))
           (->pointer (type->pointer type))
           (return-address (gl:->pointer return))
           (length (* (type->bytes type)
                      (vertex-attribute-number attr))))
      (lambda (particle)
        ;; return pointer must be fetched each call, or else a GC will throw things off
        (particle-getter particle (->pointer return) offset length)
        return))))

(define-syntax bind-mesh-accessors
  (ir-macro-transformer
   (lambda (e i c)
     (let* ((mesh (cadr e))
            (attributes (strip-syntax (caddr e)))
            (getters (map (lambda (a) (cons (i (car a)) (cdr a)))
                          (map make-getter attributes)))
            (setters (map (lambda (a) (cons (i (car a)) (cdr a)))
                          (map make-setter attributes)))
            (body (cdddr e)))
       `(let* ((vertex-attributes (mesh-vertex-attributes ,mesh))
               ,@getters ,@setters)
          ,@body)))))

(define (mesh-update* mesh)
  (let ((usage (usage->gl #:stream))
        (stride (mesh-stride mesh))
        (index-stride (type->bytes (mesh-index-type mesh)))
        (n-vertices (mesh-n-vertices mesh))
        (n-indices (mesh-n-indices mesh)))
    (gl:bind-buffer gl:+array-buffer+ (mesh-vertex-buffer mesh))
    (gl:buffer-data gl:+array-buffer+
                    (* stride n-vertices)
                    (bytevector->pointer (mesh-vertex-data mesh))
                    usage)
    (gl:bind-buffer gl:+array-buffer+ 0)
    (gl:bind-buffer gl:+element-array-buffer+ (mesh-index-buffer mesh))
    (gl:buffer-data gl:+element-array-buffer+
                   (* index-stride n-indices)
                   (bytevector->pointer (mesh-index-data mesh))
                   usage)
   (gl:bind-buffer gl:+element-array-buffer+ 0)))

(define-syntax for-emitter
  (syntax-rules ()
    ((for-emitter (var emitter attributes) new update)
     (let ((mesh (emitter-mesh emitter))
           (%emitter (emitter-emitter emitter)))
       (bind-mesh-accessors mesh attributes
         new
         (%emitter-reset %emitter)
         (do ((var (%emitter-next-particle %emitter)
                   (%emitter-next-particle %emitter)))
             ((not var))
           update))
       (mesh-update* mesh)
       (%update-emitter %emitter)))))
) ; end module hypergiant-particles
