(export cylinder-mesh
        mesh-append
        make-mesh
        mesh-index-data
        mesh-vertex-data
        mesh-transform)

(define-syntax vector-multi-set
  (ir-macro-transformer
   (lambda (x i c)
     (let* ((tag (strip-syntax (cadr x)))
            (tagstr (symbol->string tag))
            (name (symbol-append tag '-multi-set!))
            (set (symbol-append tag '-set!)))
       `(define (,name vector start . args)
          (for-each (lambda (value i)
                      (,set vector (+ start i) value))
                    args (iota (length args))))))))

(vector-multi-set f32vector)
(vector-multi-set u16vector)

(define-syntax vector-append
  (ir-macro-transformer
   (lambda (x i c)
     (let* ((tag (strip-syntax (cadr x)))
            (name (symbol-append tag '-append))
            (make (symbol-append 'make- tag))
            (set! (symbol-append tag '-set!))
            (ref (symbol-append tag '-ref))
            (length (symbol-append tag '-length)))
       `(define (,name v1 v2 . v-rest)
          (let* ((vectors (cons* v1 v2 v-rest))
                 (result (,make
                          (fold (lambda (v n)
                                  (+ (,length v) n))
                                0
                                vectors))))
            (let loop ((vectors vectors) (i 0))
              (if (null? vectors)
                  result
                  (begin
                    (let* ((v (car vectors))
                           (len (,length v)))
                      (do ((j 0 (add1 j)))
                          ((= j len))
                        (,set! result (+ i j)
                               (,ref v j)))
                      (loop (cdr vectors) (+ i len))))))))))))

(vector-append f32vector)
(vector-append u16vector)

(define f32vector->pointer
  (foreign-lambda* c-pointer ((f32vector v) (int offset))
    "C_return(v + offset);"))

(define-record mesh
  vertex-data index-data transform)

(define (mesh-append mesh1 mesh2 . meshes)
  (let* ((meshes (cons* mesh1 mesh2 meshes))
         (vertex-data (apply f32vector-append (map mesh-vertex-data meshes)))
         (index-data (apply u16vector-append (map mesh-index-data meshes))))
    (let loop ((meshes meshes) (index-index 0) (vertex-index 0))
      (if (null? meshes)
          (make-mesh vertex-data index-data (mat4-identity))
          (begin
            (let* ((mesh (car meshes))
                   (vertices (mesh-vertex-data mesh))
                   (transform (mesh-transform mesh))
                   (indices (mesh-index-data mesh))
                   (vertex-length (f32vector-length vertices))
                   (index-length (u16vector-length indices)))
              (m*vector-array! transform (f32vector->pointer vertex-data vertex-index)
                               length: (quotient vertex-length 3))
              (do ((i 0 (add1 i))
                   (vertex-offset (quotient vertex-index 3)))
                  ((= i index-length))
                (u16vector-set! index-data (+ index-index i)
                                (+ (u16vector-ref index-data (+ index-index i))
                                   vertex-offset)))
              (loop (cdr meshes)
                    (+ index-index index-length)
                    (+ vertex-index vertex-length))))))))

(define (cylinder-mesh vertical-subdivisions resolution)
  (let* ((stride 3)
         (vertex-data (make-f32vector (* resolution (add1 vertical-subdivisions)
                                         stride)))
         (index-data (make-u16vector (* resolution vertical-subdivisions 6)))
         (angle-increment (/ (* 2 pi) resolution))
         (height-increment (/ vertical-subdivisions))
         (circle-points (map (lambda (angle)
                               (list (cos angle) (sin angle)))
                             (iota resolution 0 angle-increment))))
    (dotimes (i (add1 vertical-subdivisions))
      (let* ((y (* i height-increment)))
        (dotimes (j resolution)
          (let ((circle-point (list-ref circle-points j)))
            (f32vector-multi-set! vertex-data (+ (* i resolution stride)
                                                 (* j stride))
                                  (car circle-point)
                                  y
                                  (cadr circle-point))))))
    (dotimes (i vertical-subdivisions)
      (dotimes (column resolution)
        (let* ((row (* i resolution))
               (next-row (* (add1 i) resolution))
               (next-column (if (= column (sub1 resolution))
                                0
                                (add1 column)))
               (bottom-right (+ row column))
               (bottom-left (+ row next-column))
               (top-right (+ next-row column))
               (top-left (+ next-row next-column)))
          (u16vector-multi-set! index-data (+ (* i resolution 6) (* column 6))
                                bottom-left
                                bottom-right
                                top-right
                                bottom-left
                                top-right
                                top-left))))
    (make-mesh vertex-data index-data (mat4-identity))))
