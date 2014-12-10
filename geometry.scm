(export cylinder-mesh
        rectangle-mesh)

;; (define (circle-mesh resolutions))
;; (define (cube-mesh ))
;; (define (line-mesh points))

(define (cylinder-mesh vertical-subdivisions resolution)
  (let* ((angle-increment (/ (* 2 pi) resolution))
         (height-increment (/ vertical-subdivisions))
         (circle-points (map (lambda (angle)
                               (list (cos angle) (sin angle)))
                             (iota resolution 0 angle-increment)))
         (vertices (append-ec (:range y 0 (* (add1 vertical-subdivisions)
                                             height-increment)
                                      height-increment)
                              (: j resolution)
                              (let ((circle-point (list-ref circle-points j)))
                                (list (car circle-point)
                                      y
                                      (cadr circle-point)))))
         (indices (append-ec (: i vertical-subdivisions)
                             (: column resolution)
                             (let* ((row (* i resolution))
                                    (next-row (* (add1 i) resolution))
                                    (next-column (if (= column (sub1 resolution))
                                                     0
                                                     (add1 column)))
                                    (bottom-right (+ row column))
                                    (bottom-left (+ row next-column))
                                    (top-right (+ next-row column))
                                    (top-left (+ next-row next-column)))
                               (list bottom-left
                                     bottom-right
                                     top-right
                                     bottom-left
                                     top-right
                                     top-left)))))
    (make-mesh vertices: `(attributes: ((position #:float 3))
                           initial-elements: ((position . ,vertices)))
               indices: `(type: #:uint
                          initial-elements: ,indices))))

(define (rectangle-mesh w h #!key (centered #t) color
                        (index-type #:ushort) (color-type #:ushort)
                        texture-width texture-height (texture-offset (list 0 0))
                        (texture-type #:ushort))
  (let* ((attributes (remove not (list '(position #:float 3)
                                     (and color `(color ,color-type 3
                                                      normalized: #t))
                                     (and texture-width texture-height
                                        `(tex-coord ,texture-type 2
                                                    normalized: #t)))))
         (position (cons 'position (if centered
                                       (let* ((x2 (* w 0.5))
                                              (y2 (* h 0.5))
                                              (x (- x2))
                                              (y (- y2)))
                                         (list x y 0
                                               x2 y 0
                                               x2 y2 0
                                               x y2 0))
                                       (list 0 0 0
                                             w 0 0
                                             w h 0
                                             0 h 0))))
         (color (and color (cons 'color (flatten (list-tabulate 4 color)))))
         (texture (and texture-width texture-height
                     (let* ((u (car texture-offset))
                            (v (cadr texture-offset))
                            (u2 (+ u texture-width))
                            (v2 (+ v texture-height)))
                       (cons 'tex-coord
                             (list u v2
                                   u2 v2
                                   u2 v
                                   u v)))))
         (initial-elements (remove not (list position
                                           color
                                           texture))))
    (make-mesh vertices: `(attributes: ,attributes
                           initial-elements: ,initial-elements)
               indices: `(type: ,index-type
                          initial-elements: (0 1 2
                                             0 2 3)))))
