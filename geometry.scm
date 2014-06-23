(export cylinder-mesh)

(define-syntax vector-multi-set
  (ir-macro-transformer
   (lambda (x i c)
     (let* ((tag (strip-syntax (cadr x)))
            (tagstr (symbol->string tag))
            (name (string->symbol (string-append tagstr "-multi-set!")))
            (set (string->symbol (string-append tagstr "-set!"))))
       `(define (,name vector start . args)
          (for-each (lambda (value i)
                      (,set vector (+ start i) value))
                    args (iota (length args))))))))

(vector-multi-set f32vector)
(vector-multi-set u16vector)

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
    (values vertex-data index-data)))
