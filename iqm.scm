(export load-iqm
        iqm-global-flags
        vertex-array-flags
        animation-flags
        normalized-attributes
        iqm?
        iqm-meshes
        iqm-vertex-arrays
        iqm-n-vertexes
        iqm-n-triangles
        iqm-triangles
        iqm-adjacencies
        iqm-joints
        iqm-animations
        iqm-base-frame ;; Exported for tests
        iqm-inverse-base-frame ;; Exported for tests
        iqm-flags
        iqm-comment
        iqm->mesh
        iqm->meshes)

;;; IQM loading
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
  (tx float little) (ty float little) (tz float little)
  (qx float little) (qy float little) (qz float little) (qw float little)
  (sx float little) (sy float little) (sz float little))

(bitpacket iqm-pose
  (parent 32 little signed)
  (channel-mask 32 little unsigned)
  (offset-tx float little) (offset-ty float little) (offset-tz float little)
  (offset-qx float little) (offset-qy float little) (offset-qz float little)
  (offset-qw float little)
  (offset-sx float little) (offset-sy float little) (offset-sz float little)
  (scale-tx float little) (scale-ty float little) (scale-tz float little)
  (scale-qx float little) (scale-qy float little) (scale-qz float little)
  (scale-qw float little)
  (scale-sx float little) (scale-sy float little) (scale-sz float little))

(bitpacket iqm-animation
  (name 32 little unsigned)
  (first-frame 32 little unsigned) (n-frames 32 little unsigned)
  (framerate float little)
  (flags 32 little unsigned))

(bitpacket iqm-bound
  (bb-min-x float little) (bb-min-y float little) (bb-min-z float little)
  (bb-max-x float little) (bb-max-y float little) (bb-max-z float little)
  (xy-radius float little) (radius float little))

(define iqm-custom 16)
(define vertex-array-types '(position tex-coord normal tangent
                                      blend-indexes blend-weights color))
(define vertex-array-formats '(#:char #:uchar #:short #:ushort
                                      #:int #:uint #:half #:float #:double))
(define normalized-attributes (make-parameter '(blend-weights color)))
(define vertex-array-flags (make-parameter '()))
(define iqm-global-flags (make-parameter '()))
(define animation-flags (make-parameter '((#:loop . 1))))

(define (get-flags flags defs)
  (let loop ((defs (defs)) (i 0) (acc '()))
    (if (null? defs)
        acc
        (loop (cdr defs) (add1 i)
              (if (not (zero? (bitwise-and flags (arithmetic-shift 1 i))))
                  (cons (car defs) acc)
               acc)))))

(define iqm-file (make-parameter #f))
(define text (make-parameter #f))
(define n-vertices (make-parameter #f))

(define (offset->bitstring offset)
  (if (not (zero? offset))
      (bitstring-share (iqm-file) (byte->bit offset)
                       (bitstring-end (iqm-file)))
      #f))

;; length is a function that returns a integer representing number of bytes
(define (bitstring-map getter n bs #!key length next)
  (if bs
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
                                     (bitstring-end bs))))))
      '()))

(define (bitstring->string* bs)
  ((foreign-lambda* c-string ((u8vector str) (size_t offset))
     "C_return(&str[offset]);")
   (bitstring-buffer bs) (bit->byte (bitstring-start bs))))

(define (offset->string offset)
  (let ((s (bitstring->string* (offset->bitstring (+ (text) offset)))))
    (and (not (string=? s "")) s)))

(define (bitstring->vector bs type n)
  (let ((vector ((type->make-vector type) n))
        (start (bit->byte (bitstring-start bs)))
        (length (* (gl:type->bytes type) n)))
    ((foreign-lambda* void ((c-pointer dest) (u8vector source)
                            (size_t offset) (size_t n))
       "memcpy(dest, (void *) &source[offset], n);")
     (gl:->pointer vector) (bitstring-buffer bs)
     start length)
    vector))

(define (bitstring->mesh bs)
  (bitmatch bs
    (((iqm-mesh bitpacket)
      (_ bitstring))
     `((name . ,(offset->string name))
       (material . ,(offset->string material))
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
                     (string->symbol (offset->string (- type iqm-custom)))))
         (flags . ,(get-flags flags vertex-array-flags))
         (format . ,format)
         (size . ,size)
         (array . ,(bitstring->vector
                    (offset->bitstring offset)
                    format (* size (n-vertices)))))))
    (else (error 'load-iqm "Poorly formed vertex array"))))

(define (bitstring->joint bs)
  (bitmatch bs
    (((iqm-joint bitpacket)
      (_ bitstring))
     `((name . ,(offset->string name))
       (parent . ,parent)
       (translate . ,(f32vector tx ty tz))
       (rotate . ,(f32vector qx qy qz qw))
       (scale . ,(f32vector sx sy sz))))
    (else (error 'load-iqm "Poorly formed joint"))))

(define (bitstring->pose bs)
  (bitmatch bs
    (((iqm-pose bitpacket)
      (_ bitstring))
     `((parent . ,parent)
       (channel-mask . ,channel-mask)
       (channel-offset . ,(f32vector offset-tx offset-ty offset-tz
                                     offset-qx offset-qy offset-qz offset-qw
                                     offset-sx offset-sy offset-sz))
       (channel-scale . ,(f32vector scale-tx scale-ty scale-tz
                                    scale-qx scale-qy scale-qz scale-qw
                                    scale-sx scale-sy scale-sz))))
    (else (error 'load-iqm "Poorly formed pose"))))

(define (bitstring->animation bs)
  (bitmatch bs
    (((iqm-animation bitpacket)
      (_ bitstring))
     `((name . ,(offset->string name))
       (first-frame . ,first-frame)
       (n-frames . ,n-frames)
       (framerate . ,framerate)
       (flags . , (get-flags flags animation-flags))))
    (else (error 'load-iqm "Poorly formed animation"))))

(define (bitstring->bounds bs)
  (bitmatch bs
    (((iqm-bound bitpacket)
      (_ bitstring))
     `((bb-min . ,(f32vector bb-min-x bb-min-y bb-min-z))
       (bb-max . ,(f32vector bb-max-x bb-max-y bb-max-z))
       (xy-radius . ,xy-radius)
       (radius . ,radius)))
    (else (error 'load-iqm "Poorly formed bounds"))))

(define (get-meshes offset n)
  (bitstring-map bitstring->mesh n (offset->bitstring offset)
                 length: (lambda (_) 24)))

(define (get-vertex-arrays offset n)
  (bitstring-map bitstring->vertex-array
                 n (offset->bitstring offset)
                 length: (lambda (_) 20)))

(define (get-joints offset n)
  (bitstring-map bitstring->joint
                 n (offset->bitstring offset)
                 length: (lambda (_) 48)))

(define (get-poses offset n)
  (bitstring-map bitstring->pose
                 n (offset->bitstring offset)
                 length: (lambda (_) 88)))

(define (get-animations offset n)
  (bitstring-map bitstring->animation
                 n (offset->bitstring offset)
                 length: (lambda (_) 20)))

(define (get-bounds offset n)
  (bitstring-map bitstring->bounds
                 n (offset->bitstring offset)
                 length: (lambda (_) 32)))

(define (get-vertex-array vertex-arrays attribute)
  (if* (find (lambda (va) (equal? (alist-ref 'type va)
                             attribute))
             vertex-arrays)
       it
       (error 'load-iqm "No such attribute in IQM:" attribute)))

(define (get-triangles offset n)
  (if* (offset->bitstring offset)
       (bitstring->vector
        (offset->bitstring offset) #:uint (* n 3))
       '()))

(define (get-frames offset n-frames n-frame-channels)
  (if (not (zero? offset))
      (bitstring->vector (offset->bitstring offset)
                         #:ushort
                         (* n-frames n-frame-channels))
      '()))

(define (build-poses poses frames n-frames)
  (define frame-counter 0)
  (define (pop-frame)
    (begin0 (u16vector-ref frames frame-counter)
      (inc! frame-counter)))
  (map (lambda (frame)
         (map (lambda (pose)
                (let* ((mask (alist-ref 'channel-mask pose))
                       (offset (alist-ref 'channel-offset pose))
                       (scale (alist-ref 'channel-scale pose))
                       (r (map (lambda (c)
                                 (+ (f32vector-ref offset c)
                                    (if (zero? (bitwise-and
                                                mask
                                                (arithmetic-shift 1 c)))
                                        0
                                        (* (pop-frame)
                                           (f32vector-ref scale c)))))
                               (iota 10))))
                  `((translate . ,(f32vector (first r) (second r) (third r)))
                    (rotate . ,(f32vector (fourth r) (fifth r) (sixth r)
                                         (seventh r)))
                    (scale . ,(f32vector (eighth r) (ninth r) (tenth r)))
                    (parent . ,(alist-ref 'parent pose)))))
              poses))
       (iota n-frames)))

(define (build-frame-matrix-array array poses frames
                                  base-frame inverse-base-frame
                                  n-frames n-poses)
  (for-each
   (lambda (poses i)
     (for-each
      (lambda (pose j)
        (let* ((quat (alist-ref 'rotate pose))
               (trans (alist-ref 'translate pose))
               (scale (alist-ref 'scale pose))
               (parent (alist-ref 'parent pose))
               (m (gl:->pointer (translate
                                 trans
                                 (rotate-quaternion
                                  quat
                                  (3d-scaling (f32vector-ref scale 0)
                                              (f32vector-ref scale 1)
                                              (f32vector-ref scale 2))))))
               (dest (nth-matrix array
                                 (+ (* i n-poses) j))))
          (if (>= parent 0)
              (let ((m*inverse (gl:->pointer (make-f32vector 16))))
                (m* m (nth-matrix inverse-base-frame j)
                    m*inverse)
                (m* (nth-matrix base-frame parent)
                    m*inverse
                    dest))
              (m* m (nth-matrix inverse-base-frame j)
                  dest))))
      poses (iota n-poses)))
   poses (iota n-frames)))

(define (build-animations animations poses frames bounds
                         base-frame inverse-base-frame n-frames)
  (let ((poses (build-poses poses frames n-frames))
        (n-poses (length poses))
        (bounds (and (not (null? bounds)) bounds))
        (parents (map (cut alist-ref 'parent <>)
                      poses)))
    (map
     (lambda (a)
       (let* ((first-frame (alist-ref 'first-frame a))
              (n-frames (alist-ref 'n-frames a))
              (poses  (take (drop poses first-frame)
                            n-frames))
              (frame-matrix-array (make-matrix-array (* n-frames n-poses))))
         (build-frame-matrix-array frame-matrix-array poses frames
                                   base-frame inverse-base-frame
                                   n-frames n-poses)
         (cons (string->symbol (alist-ref 'name a))
               (make-animation (if (member #:loop (alist-ref 'flags a))
                                   #:loop
                                   #:once)
                               (cons frame-matrix-array
                                     (list->vector parents))
                               (/ (alist-ref 'framerate a) 60)
                               bounds: (and bounds (take (drop bounds first-frame)
                                                         n-frames))
                               n-frames: n-frames))))
     animations)))

(define (calc-base-frame base-frame inverse-base-frame joints)
  (do ((joints joints (cdr joints))
       (i 0 (add1 i)))
      ((null? joints))
    (let* ((joint (car joints))
           (scale (alist-ref 'scale joint))
           (trans (alist-ref 'translate joint))
           (rot (alist-ref 'rotate joint))
           (temp-m
            (translate
             trans
             (rotate-quaternion
              rot
              (3d-scaling (f32vector-ref scale 0)
                          (f32vector-ref scale 1)
                          (f32vector-ref scale 2)))))
           (temp-m-inverse (inverse temp-m))
           (m-base-frame (nth-matrix base-frame i))
           (m-inverse-base-frame (nth-matrix inverse-base-frame i))
           (parent (alist-ref 'parent joint)))
      (if (>= parent 0)
          (begin
            (m* (nth-matrix base-frame parent)
                (gl:->pointer temp-m)
                m-base-frame)
            (m* (gl:->pointer temp-m-inverse)
                (nth-matrix inverse-base-frame parent)
                m-inverse-base-frame))
          (begin
            (copy-mat4 (gl:->pointer temp-m) m-base-frame)
            (copy-mat4 (gl:->pointer temp-m-inverse)
                       m-inverse-base-frame))))))

(define-record-type iqm
  #t #t
  (meshes) (vertex-arrays) (n-vertexes) (n-triangles) (triangles) (adjacencies)
  (joints) (base-frame) (inverse-base-frame) (animations) (flags) (comment))

(define-record-printer (iqm iqm out)
  (fprintf out "#<iqm vertexes: ~S triangles: ~S~%     meshes: ~S~%     vertex-arrays: ~S~%     joints: ~S~%     animations: ~S~%     flags: ~S~%     comment: ~S>"
    (iqm-n-vertexes iqm) (iqm-n-triangles iqm)
    (iqm-meshes iqm)
    (map (cut alist-ref 'type <>) (iqm-vertex-arrays iqm))
    (map (lambda (j)
           (list (alist-ref 'name j)
                 (alist-ref 'parent j)))
         (iqm-joints iqm))
    (map car (iqm-animations iqm))
    (iqm-flags iqm) (iqm-comment iqm)))

(define (load-iqm file #!optional base-iqm)
  (bitmatch (file->u8vector file)
    (((iqm-header bitpacket)
      (rest bitstring))
     (unless (= version 2)
       (error 'load-iqm "Only IQM version 2 is supported:" file))
     (parameterize ((iqm-file rest))
       (parameterize ((text text-offset))
         (parameterize ((n-vertices n-vertexes))
           (let ((animations (get-animations animations-offset n-animations))
                 (poses (get-poses poses-offset n-poses))
                 (frames (get-frames frames-offset n-frames n-frame-channels))
                 (bounds (get-bounds bounds-offset n-frames))
                 (comment-bitstring (offset->bitstring comment-offset))
                 (temp-base-frame #f)
                 (temp-inverse-base-frame #f)
                 (base-frame #f)
                 (inverse-base-frame #f)
                 (joints (get-joints joints-offset n-joints)))
             (cond 
              ((positive? n-joints)
               (set! base-frame (make-matrix-array n-joints))
               (set! inverse-base-frame (make-matrix-array n-joints))
               (set! temp-base-frame base-frame)
               (set! temp-inverse-base-frame inverse-base-frame)
               (calc-base-frame base-frame inverse-base-frame joints))
              (base-iqm
                (set! temp-base-frame (iqm-base-frame base-iqm))
                (set! temp-inverse-base-frame
                     (iqm-inverse-base-frame base-iqm))))
             (set-finalizer!
              (make-iqm (get-meshes meshes-offset n-meshes)
                        (get-vertex-arrays vertex-arrays-offset n-vertex-arrays)
                        n-vertexes n-triangles
                        (get-triangles triangles-offset n-triangles)
                        (get-triangles adjacency-offset n-triangles)
                        joints
                        base-frame inverse-base-frame
                        (and temp-base-frame
                             (build-animations animations poses frames bounds
                                               temp-base-frame temp-inverse-base-frame
                                               n-frames))
                        (get-flags flags iqm-global-flags)
                        (and comment-bitstring
                             (bitstring->string comment-bitstring)))
              (lambda (a)
                (when iqm-base-frame
                  (free (iqm-base-frame a))
                  (free (iqm-inverse-base-frame a))))))))))
    (else (error 'load-iqm "Poorly formed IQM file:" file))))

;;; IQM -> mesh
(define (vertex-arrays->vertex-attributes vertex-arrays attributes)
  (map (lambda (a)
         (let* ((array (get-vertex-array vertex-arrays a))
                (type (alist-ref 'type array)))
           (list type
                 (alist-ref 'format array)
                 (alist-ref 'size array)
                 normalized:
                 (if (find (cut equal? type <>) (normalized-attributes))
                     #t
                     #f))))
       attributes))

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

;;; IQM -> animated model
(define (make-iqm-animated-model node iqm base-animation-name)
  (make-animated-model node (alist-ref base-animation-name
                                       (iqm-animations iqm))
                       (length (iqm-joints iqm))))

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

(define (make-matrix-array n) (allocate (* 4 16 n)))
(define (nth-matrix m n) (pointer+ m (* 4 16 n)))
