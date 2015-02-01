(export load-iqm
        iqm->mesh
        iqm->meshes)

(define iqm-magic (bitstring-append (string->bitstring "INTERQUAKEMODEL")
                                    (->bitstring #u8(0))))

(bitpacket iqm-header
  (magic (bitstring-length iqm-magic) bitstring)
  (check (bitstring=? magic iqm-magic))
  (version 32 little unsigned)
  (file-size 32 little unsigned)
  (flags 32 little unsigned)
  (n-text 32 little unsigned) (text-offset 32 little unsigned)
  (n-meshes 32 little unsigned) (meshes-offset 32 little unsigned)
  (n-vertex-arrays 32 little unsigned) (n-vertexes 32 little unsigned)
    (vertex-arrays-offset 32 little unsigned)
  (n-triangles 32 little unsigned) (triangles-offset 32 little unsigned)
    (adjacency-offset 32 little unsigned)
  (n-joints 32 little unsigned) (joints-offset 32 little unsigned)
  (n-poses 32 little unsigned) (poses-offset 32 little unsigned)
  (n-animations 32 little unsigned) (animations-offset 32 little unsigned)
  (n-frames 32 little unsigned) (n-frame-channels 32 little unsigned)
    (frames-offset 32 little unsigned) (bounds-offset 32 little unsigned)
  (n-comment 32 little unsigned) (comment-offset 32 little unsigned)
  (n-extensions 32 little unsigned) (extensions-offset 32 little unsigned))

(bitpacket iqm-mesh
  (name 32 little unsigned)
  (material 32 little unsigned)
  (first-vertex 32 little unsigned) (n-vertexes 32 little unsigned)
  (first-triangle 32 little unsigned) (n-triangles 32 little unsigned))

(bitpacket iqm-vertex-array
  (type 32 little unsigned)
  (flags 32 little unsigned)
  (format 32 little unsigned)
  (size 32 little unsigned)
  (offset 32 little unsigned))

(bitpacket iqm-joint
  (name 32 little unsigned)
  (parent 32 little signed)
  (rest bitstring))

(bitpacket iqm-pose
  (parent 32 little signed)
  (channel-mask 32 little unsigned)
  (rest bitstring))

(bitpacket iqm-animation
  (name 32 little unsigned)
  (first-frame 32 little unsigned) (n-frames 32 little unsigned)
  (framerate floud little)
  (flags 32 little unsigned))

(define iqm-custom 16)
(define vertex-array-types '(position tex-coord normal tangent
                                      blend-indexes blend-weights color))
(define normalized-attributes (make-parameter '(blend-weights color)))
(define vertex-array-formats '(#:char #:uchar #:short #:ushort
                                      #:int #:uint #:half #:float #:double))

(define iqm-file (make-parameter #f))
(define text (make-parameter #f))
(define n-vertices (make-parameter #f))

(define (offset->bitstring offset #!optional length)
  (bitstring-share (iqm-file) (byte->bit offset)
                   (if length
                       (byte->bit (+ offset length))
                       (bitstring-end (iqm-file)))))

;; length is a function that returns a integer representing number of bytes
(define (bitstring-map getter n bs #!key length next)
  (let loop ((i 0) (acc '()) (bs bs))
    (if (>= i n)
        (reverse acc)
        (let ((el (getter bs)))
          (loop (add1 i)
                (cons el acc)
                (bitstring-share bs
                                 (if length
                                     (+ (bitstring-start bs)
                                        (byte->bit (length el)))
                                     (next el))
                                 (bitstring-end bs)))))))

(define (bitstring->string* bs)
  ((foreign-lambda* c-string ((u8vector str) (size_t offset))
     "C_return(&str[offset]);")
   (bitstring-buffer bs) (bit->byte (bitstring-start bs))))

(define (bitstring->vector bs type n)
  (let ((vector ((type->make-vector type) n))
        (start (bit->byte (bitstring-start bs)))
        (end (bit->byte (bitstring-end bs))))
    ((foreign-lambda* void ((c-pointer dest) (u8vector source)
                            (size_t offset) (size_t n))
       "memcpy(dest, (void *) &source[offset], n);")
     (gl:->pointer vector) (bitstring-buffer bs)
     start (- end start))
    vector))

(define (bitstring->mesh bs)
  (bitmatch bs
    (((iqm-mesh bitpacket)
      (_ bitstring))
     `((name . ,(list-ref (text) name))
       (material . ,(list-ref (text) material))
       (first-vertex . ,first-vertex)
       (n-vertexes . ,n-vertexes)
       (first-triangle . ,first-triangle)
       (n-triangles . ,n-triangles)))
    (else (error 'load-iqm "Poorly formed mesh"))))

(define (bitstring->vertex-array bs)
  (bitmatch bs
    (((iqm-vertex-array bitpacket)
      (_ bitstring))
     (let ((format (list-ref vertex-array-formats format)))
       `((type . ,(if (< type iqm-custom)
                     (list-ref vertex-array-types type)
                     (string->symbol (list-ref (text) (- type iqm-custom)))))
         (flags . ,flags)
         (format . ,format)
         (size . ,size)
         (array . ,(bitstring->vector
                    (offset->bitstring offset
                                       (* size (n-vertices)
                                          (gl:type->bytes format)))
                    format (* size (n-vertices)))))))
    (else (error 'load-iqm "Poorly formed vertex array"))))

(define (get-text offset n)
  (map (lambda (s) (and (not (string=? s "")) s))
       (bitstring-map bitstring->string*
                      n (offset->bitstring offset)
                      length: (lambda (s) (add1 (string-length s))))))

(define (get-meshes offset n)
  (bitstring-map bitstring->mesh n (offset->bitstring offset)
                 length: (lambda (_) 24)))

(define (get-vertex-arrays offset n)
  (bitstring-map bitstring->vertex-array
                 n (offset->bitstring offset)
                 length: (lambda (_) 20)))

(define (get-vertex-array vertex-arrays attribute)
  (if* (find (lambda (va) (equal? (alist-ref 'type va)
                             attribute))
             vertex-arrays)
       it
       (error 'load-iqm "No such attribute in IQM:" attribute)))

(define (vertex-arrays->vertex-attributes vertex-arrays attributes)
  (map (lambda (a)
         (let* ((array (get-vertex-array vertex-arrays a))
                (type (alist-ref 'type array)))
           (list type
                 (alist-ref 'format array)
                 (alist-ref 'size array)
                 (find (cut equal? type <>) (normalized-attributes)))))
       attributes))

(define-record-type iqm
  #t #t
  (meshes) (vertex-arrays) (n-vertexes) (n-triangles)
  (triangles) (adjacencies)
  ;TODO (joints) (poses) (animations) ;(frames) (bounds) part of animation?
  )

(define-record-printer (iqm iqm out)
  (fprintf out "#<iqm vertexes: ~S triangles ~S meshes: ~S vertex-arrays: ~S>"
    ;; TODO
    (iqm-n-vertexes iqm) (iqm-n-triangles iqm)
    (iqm-meshes iqm) (map (cut alist-ref 'type <>) (iqm-vertex-arrays iqm))))

(define (load-iqm file)
  (bitmatch (file->u8vector file)
    (((iqm-header bitpacket)
      (rest bitstring))
     (unless (= version 2)
       (error 'load-iqm "Only IQM version 2 is supported:" file))
     (parameterize ((iqm-file rest))
       (parameterize ((text (get-text text-offset n-text)))
         (parameterize ((n-vertices n-vertexes))
           (make-iqm (get-meshes meshes-offset n-meshes)
                     (get-vertex-arrays vertex-arrays-offset n-vertex-arrays)
                     n-vertexes n-triangles
                     (bitstring->vector
                      (offset->bitstring triangles-offset
                                         (* 3 n-triangles
                                            (gl:type->bytes #:uint)))
                      #:uint (* n-triangles 3))
                     (bitstring->vector
                      (offset->bitstring adjacency-offset
                                         (* 3 n-triangles
                                            (gl:type->bytes #:uint)))
                      #:uint (* n-triangles 3))
                     ;; TODO more
                     )))))
    (else (error 'load-iqm "Poorly formed IQM file:" file))))

(define (index-copy mesh iqm)
  (let ((n-triangles (iqm-n-triangles iqm)))
    ((foreign-lambda* void ((u8vector dest) (c-pointer source) (size_t n))
       "memcpy((void *) dest, source, n);")
     (gl:mesh-index-data mesh) (gl:->pointer (iqm-triangles iqm))
     (* 3 n-triangles (gl:type->bytes #:uint)))))

(define (vertex-copy mesh iqm
                    #!optional (n-vertexes (iqm-n-vertexes iqm)) (first-vertex 0))
  (let loop ((attributes (gl:mesh-vertex-attributes mesh)))
    (unless (null? attributes)
      (let* ((attribute (car attributes))
             (vertex-array (find (lambda (va)
                                   (equal? (alist-ref 'type va)
                                           (gl:vertex-attribute-name attribute)))
                                 (iqm-vertex-arrays iqm)))
             (array (alist-ref 'array vertex-array))
             (vertex-offset (gl:vertex-attribute-offset attribute))
             (mesh-stride (gl:mesh-stride mesh))
             (size (* (gl:type->bytes (gl:vertex-attribute-type attribute))
                      (gl:vertex-attribute-number attribute))))
        (dotimes (vertex n-vertexes)
          ((foreign-lambda* void ((u8vector dest) (c-pointer source) (size_t n)
                                  (size_t dest_offset) (size_t source_offset))
             "memcpy((void *) &dest[dest_offset],  
       (void *) &((char *) source)[source_offset], n);")
           (gl:mesh-vertex-data mesh) (gl:->pointer array)
           size
           (+ (* vertex mesh-stride) vertex-offset)
           (* (+ vertex first-vertex) size))))
      (loop (cdr attributes)))))

(define (index-shift-copy mesh iqm n-triangles first-triangle first-vertex)
  (let ((dest (gl:mesh-index-data mesh))
        (source (iqm-triangles iqm)))
    (dotimes (i (* 3 n-triangles))
      (gl:bytevector-u32-set! dest (* i 4)
                              (- (u32vector-ref source (+ (* first-triangle 3) i))
                                 first-vertex)))))

(define (iqm->mesh iqm attributes)
  (let* ((attributes (vertex-arrays->vertex-attributes (iqm-vertex-arrays iqm)
                                                       attributes))
         (mesh (gl:make-mesh vertices: `(attributes: ,attributes
                                         n-vertices: ,(iqm-n-vertexes iqm))
                             indices: `(type: #:uint
                                        n-indices: ,(* 3 (iqm-n-triangles iqm))))))
    (index-copy mesh iqm)
    (vertex-copy mesh iqm)
    mesh))

(define (iqm->meshes iqm attributes)
  (let ((attributes (vertex-arrays->vertex-attributes (iqm-vertex-arrays iqm)
                                                      attributes)))
    (map (lambda (iqm-mesh)
           (let ((mesh (gl:make-mesh
                        vertices: `(attributes: ,attributes
                                    n-vertices: ,(alist-ref 'n-vertexes iqm-mesh))
                        indices: `(type: #:uint
                                   n-indices: ,(* 3 (alist-ref 'n-triangles iqm-mesh))))))
             (index-shift-copy mesh iqm
                               (alist-ref 'n-triangles iqm-mesh)
                               (alist-ref 'first-triangle iqm-mesh)
                               (alist-ref 'first-vertex iqm-mesh))
             (vertex-copy mesh iqm
                          (alist-ref 'n-vertexes iqm-mesh)
                          (alist-ref 'first-vertex iqm-mesh))
             mesh))
         (iqm-meshes iqm))))

;;; Utils
(define (bit->byte x) (arithmetic-shift x -3))
(define (byte->bit x) (arithmetic-shift x 3))

(define (file->u8vector file)
  (with-input-from-file file
    (lambda ()
      (read-u8vector #f))))

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
