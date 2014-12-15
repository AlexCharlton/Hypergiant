(export cylinder-mesh
        rectangle-mesh
        circle-mesh
        sphere-mesh
        cube-mesh
        line-mesh)

(define (index-winding indices winding)
  (ecase winding
    ((#:ccw) indices)
    ((#:cw) (let loop ((indices indices))
              (if (null? indices)
                  '()
                  (cons* (caddr indices) (cadr indices) (car indices)
                         (loop (cdddr indices))))))))

(define (mesh-make vertices indices winding index-type color-type texture-type
                   texture-dims mode)
  (let ((attributes (list '(position #:float 3)
                          '(normal #:float 3)
                          `(color ,color-type 3 normalized: #t)
                          `(tex-coord ,texture-type ,texture-dims normalized: #t))))
    (make-mesh vertices: `(attributes: ,attributes
                           initial-elements: ,(remove not vertices))
               indices: `(type: ,index-type
                          initial-elements: ,(index-winding indices winding))
               mode: mode)))

(define (build-color color n-verts)
  (and color (cons 'color (flatten (list-tabulate n-verts color)))))

(define (build-texture texture width height offset n-u n-v)
  (cond
   (texture (cons 'tex-coord (flatten (list-tabulate (* n-u n-v) texture))))
   ((and width height) (cons 'tex-coord
                             (let ((offset-u (car offset))
                                   (offset-v (cadr offset)))
                               (append-ec (:real-range v offset-v
                                                       (+ offset-v height 0.00001)
                                                       (/ height (sub1 n-v)))
                                          (:real-range u offset-u
                                                       (+ offset-u width 0.00001)
                                                       (/ width (sub1 n-u)))
                                          (list u v)))))
   (else #f)))

(define (line-mesh points #!key (mode #:line-strip))
  (make-mesh vertices: `(attributes: (position #:float 3)
                         initial-elements: ,points)
             mode: mode))

(define (rectangle-mesh w h #!key (centered #t) color (mode #:triangles) (winding #:ccw)
                        (index-type #:ushort) (color-type #:ushort)
                        (texture-type #:ushort) texture
                        texture-width texture-height (texture-offset (list 0 0)))
  (let* ((position (cons 'position (if centered
                                       (let* ((x2 (* w 0.5))
                                              (y2 (* h 0.5))
                                              (x (- x2))
                                              (y (- y2)))
                                         (list x y2 0
                                               x2 y2 0
                                               x y 0
                                               x2 y 0))
                                       (list 0 h 0
                                             w h 0
                                             0 0 0
                                             w 0 0))))
         (color (build-color color 4))
         (texture (build-texture texture texture-width texture-height texture-offset
                                 2 2)))
    (mesh-make (list position
                     color
                     texture)
               '(0 2 1 1 2 3)
               winding
               index-type
               color-type
               texture-type
               2
               mode)))

(define (circle-mesh radius resolution #!key (mode #:triangles) color (winding #:ccw)
                     (index-type #:ushort) (color-type #:ushort)
                     (texture-type #:ushort) texture texture-radius
                     (texture-offset (list 0.5 0.5)))
  (when (< resolution 3)
    (error 'circle-mesh "resolution not be less than 3:" resolution))
  (let* ((angle-increment (/ (* 2 pi) resolution))
         (unit-circle (map (lambda (angle)
                             (cons (cos angle) (sin angle)))
                             (iota resolution 0 angle-increment)))
         (position  (cons 'position
                          (append '(0 0 0)
                                  (append-ec (:list point unit-circle)
                                             (list (* radius (car point))
                                                   (* radius (cdr point))
                                                   0)))))
         (indices (append (list resolution 1 0)
                          (append-ec (:range i (sub1 resolution))
                                     (list (+ i 1)
                                           (+ i 2)
                                           0))))
         (color (build-color color (add1 resolution)))
         (texture (cond
                   (texture (cons 'tex-coord (flatten (list-tabulate (add1 resolution)
                                                                     texture))))
                   (texture-radius
                    (cons 'tex-coord
                          (let ((offset-u (car texture-offset))
                                (offset-v (cadr texture-offset)))
                            (append (list offset-u offset-v)
                                    (append-ec (:list point unit-circle)
                                               (list (+ offset-u (car point))
                                                     (+ offset-v (cadr point))))))))
                   (else #f))))
    (mesh-make (list position
                     color
                     texture)
               indices
               winding
               index-type
               color-type
               texture-type
               2
               mode)))

; TODO
(define (cube-mesh length #!key (mode #:triangles) (normals? #f) (winding #:ccw)
                     (index-type #:ushort) (color-type #:ushort) (texture-type #:ushort))
  #f)

(define (sphere-mesh radius resolution #!key (type #:uv) color (normals? #f)
                     (index-type #:ushort) (color-type #:ushort)
                     (texture-type #:ushort) (mode #:triangles) (winding #:ccw)
                     texture texture-width texture-height (texture-offset (list 0 0)))
  (ecase type
    ((#:uv) (uv-sphere-mesh radius resolution normals? color mode winding
                            texture texture-width texture-height texture-offset
                            index-type color-type texture-type))
    ((#:iso) (isosphere-mesh radius resolution normals? color mode winding
                             ;texture texture-width texture-height texture-offset
                             index-type color-type texture-type))))

;; TODO
(define (isosphere-mesh radius resolution normals? color mode winding
                        ;texture texture-width texture-height texture-offset
                        index-type color-type texture-type)
  #f)

(use format)
(define (uv-sphere-mesh radius resolution normals? color mode winding
                        texture texture-width texture-height texture-offset
                        index-type color-type texture-type)
  (when (or (odd? resolution) (< resolution 4))
    (error 'uv-sphere-mesh "Resolution must be even and not less than 4:" resolution))
  (let* ((angle-increment (* 2 (/ pi resolution)))
         (n-lat-verts (add1 (/ resolution 2)))
         (circle-points (map (lambda (angle)
                               (cons (cos angle) (- (sin angle))))
                             (iota (add1 resolution) 0 angle-increment)))
         (semi-circle-points (map (lambda (angle)
                                    (cons (cos angle)
                                          (sin angle)))
                                  (iota n-lat-verts 0 angle-increment)))
         (unit-sphere (append-ec (:list latitude semi-circle-points)
                                 (:list longitude circle-points)
                                 (list (* (car longitude) (cdr latitude))
                                       (car latitude)
                                       (* (cdr longitude) (cdr latitude)))))
         (vertices (map (cut * radius <>)
                        unit-sphere))
         (indices (append-ec (:range lat (/ resolution 2))
                             (:range column resolution)
                             (let* ((row (* lat (add1 resolution)))
                                    (next-row (+ row (add1 resolution)))
                                    (next-column (add1 column))
                                    (top-left (+ row column))
                                    (top-right (+ row next-column))
                                    (bottom-left (+ next-row column))
                                    (bottom-right (+ next-row next-column)))
                               (list top-left
                                     bottom-left
                                     bottom-right
                                     top-left
                                     bottom-right
                                     top-right))))
         (position (cons 'position vertices))
         (normal (and normals? (cons 'normal unit-sphere)))
         (color (and color (cons 'color
                                 (flatten (list-tabulate (* (add1 resolution)
                                                            n-lat-verts)
                                                         color)))))
         (texture (build-texture texture texture-width texture-height texture-offset
                                 (add1 resolution) n-lat-verts)))
    ;; (let loop ((t (cdr texture))
    ;;            (p (cdr position))
    ;;            (i 1))
    ;;   (unless (null? t)
    ;;     (format #t "(~$ ~$ ~$) (~$ ~$)~%"
    ;;             (car p) (cadr p) (caddr p)
    ;;             (car t) (cadr t))
    ;;     (when (= (modulo i (add1 resolution)) 0)
    ;;       (newline))
    ;;     (loop (cddr t) (cdddr p) (add1 i))))
    (mesh-make (list position
                     normal
                     color
                     texture)
               indices
               winding
               index-type
               color-type
               texture-type
               2 ; TODO 3d texture
               mode)))

(define (cylinder-mesh length radius vertical-subdivisions resolution
                       #!key color (normals? #f) (winding #:ccw) (mode #:triangles) 
                       (index-type #:ushort) (color-type #:ushort) (texture-type #:ushort) 
                       texture texture-width texture-height (texture-offset (list 0 0)))
  (when (< resolution 3)
    (error 'cylinder-mesh "resolution not be less than 3:" resolution))
  (when (< vertical-subdivisions 1)
    (error 'cylinder-mesh "vertical-subdivisions must not be less than 1:"
           vertical-subdivisions))
  (let* ((angle-increment (/ (* 2 pi) resolution))
         (height-increment (/ length vertical-subdivisions))
         (unit-circle (map (lambda (angle)
                               (cons (cos angle)
                                     (sin angle)))
                             (iota resolution 0 angle-increment)))
         (circle-points (map (lambda (point)
                               (cons (* radius (car point))
                                     (* radius (cdr point))))
                             unit-circle))
         (position (cons 'position
                         (append-ec (:range y 0 (* (add1 vertical-subdivisions)
                                                   height-increment)
                                            height-increment)
                                    (:list circle-point circle-points)
                                    (list (car circle-point)
                                          y
                                          (cdr circle-point)))))
         (indices (append-ec (:range i vertical-subdivisions)
                             (:range column resolution)
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
                                     top-left))))
         (normal (and normals?
                      (cons 'normal
                            (append-ec (:range y 0 (* (add1 vertical-subdivisions)
                                                      height-increment)
                                               height-increment)
                                       (:list point unit-circle)
                                       (list (car point)
                                             0
                                             (cdr point))))))
         (color (and color
                     (cons 'color
                           (flatten (list-tabulate (* resolution
                                                      (add1 vertical-subdivisions))
                                                   color)))))
         (texture #f ;TODO 
          ))
    (mesh-make (list position
                     normal
                     color
                     texture)
               indices
               winding
               index-type
               color-type
               texture-type
               2 ; TODO
               mode)))
