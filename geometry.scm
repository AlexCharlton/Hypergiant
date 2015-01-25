;;;; geometry.scm

;;;; Generation of geometric primitive meshes

(module hypergiant-geometry
(cylinder-mesh
 rectangle-mesh
 circle-mesh
 sphere-mesh
 cube-mesh
 line-mesh)

(import chicken scheme)
(use gl-utils gl-math srfi-1 srfi-42 miscmacros data-structures)

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
  (let* ((vertices (remove not vertices))
         (attributes (list (and (alist-ref 'position vertices)
                                '(position #:float 3))
                           (and (alist-ref 'normal vertices)
                                '(normal #:float 3))
                           (and (alist-ref 'color vertices)
                                `(color ,color-type 3 normalized: #t))
                           (and (alist-ref 'tex-coord vertices)
                                `(tex-coord ,texture-type ,texture-dims
                                            normalized: #t)))))
    (make-mesh vertices: `(attributes: ,(remove not attributes)
                                       initial-elements: ,vertices)
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

(define (rectangle-mesh w h #!key (centered? #t) color (mode #:triangles) (winding #:ccw)
                        (index-type #:ushort) (color-type #:ushort)
                        (texture-type #:ushort) texture
                        texture-width texture-height (texture-offset (list 0 0)))
  (let* ((position (cons 'position (if centered?
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
                                               (list (+ offset-u
                                                        (* texture-radius
                                                           (car point)))
                                                     (+ offset-v
                                                        (* texture-radius
                                                           (- (cdr point))))))))))
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

(define (cube-mesh length #!key (mode #:triangles) (normals? #f) (winding #:ccw)
                   color texture cube-map?
                   (index-type #:ushort) (color-type #:ushort) (texture-type #:float))
  (let* ((front-top-left     '(-1  1  1))
         (front-top-right    '( 1  1  1))
         (front-bottom-left  '(-1 -1  1))
         (front-bottom-right '( 1 -1  1))
         (back-top-left      '(-1  1 -1))
         (back-top-right     '( 1  1 -1))
         (back-bottom-left   '(-1 -1 -1))
         (back-bottom-right  '( 1 -1 -1))
         (back               '( 0  0 -1))
         (front              '( 0  0  1))
         (left               '(-1  0  0))
         (right              '( 1  0  0))
         (up                 '( 0  1  0))
         (down               '( 0 -1  0))
         (unit-cube (append front-top-left front-top-right ; front
                            front-bottom-left front-bottom-right
                            front-top-right back-top-right ;right
                            front-bottom-right back-bottom-right
                            back-top-right back-top-left ; back
                            back-bottom-right back-bottom-left
                            back-top-left front-top-left ; left
                            back-bottom-left front-bottom-left
                            back-top-left back-top-right ; top
                            front-top-left front-top-right
                            front-bottom-left front-bottom-right ;bottom
                            back-bottom-left back-bottom-right))
         (indices (append-ec (:range i 6)
                             (let ((face (* i 4)))
                               (map (cut + face <>)
                                    (list 0 2 3 0 3 1)))))
         (half-length (* 0.5 length))
         (position (cons 'position (map (cut * half-length <>)
                                        unit-cube)))
         (normal (and normals? (cons 'normal (append front front front front
                                                     right right right right
                                                     back back back back
                                                     left left left left
                                                     up up up up
                                                     down down down down))))
         (color (and color (cons 'color
                                 (flatten (list-tabulate 24 color)))))
         (texture (cond
                   (texture (cons 'tex-coord (flatten (list-tabulate 24 texture))))
                   (cube-map?
                    (cons 'tex-coord unit-cube))
                   (else #f))))
    (mesh-make (list position
                     normal
                     color
                     texture)
               indices
               winding
               index-type
               color-type
               texture-type
               3
               mode)))

(define (sphere-mesh radius resolution #!key (type #:uv) color (normals? #f)
                     (index-type #:ushort) (color-type #:ushort) cube-map?
                     texture-type (mode #:triangles) (winding #:ccw)
                     texture texture-width texture-height (texture-offset (list 0 0)))
  (ecase type
    ((#:uv) (uv-sphere-mesh radius resolution normals? color mode winding
                            texture texture-width texture-height texture-offset
                            index-type color-type (or texture-type #:ushort)))
    ((#:cube) (cubesphere-mesh radius resolution normals? color mode winding
                               texture cube-map?
                               index-type color-type (or texture-type #:float)))))

(define (cubesphere-mesh radius resolution normals? color mode winding
                         texture cube-map? index-type color-type texture-type)
  (when (< resolution 1)
    (error 'cubesphere-mesh "Resolution must not be less than 1:" resolution))
  (define resolution+1 (add1 resolution))
  (define (gen-face top-left bottom-right)
    (define (gen-range a b)
      (if (= a b)
          a
          (iota resolution+1 a (/ (- b a) resolution))))
    (let* ((xs (gen-range (first top-left) (first bottom-right)))
           (ys (gen-range (second top-left) (second bottom-right)))
           (zs (gen-range (third top-left) (third bottom-right))))
      (cond
       ((number? xs)
        (list-ec (:list y ys)
                 (:list z zs)
                 (list xs y z)))
       ((number? ys)
        (list-ec (:list z zs)
                 (:list x xs)
                 (list x ys z)))
       ((number? zs)
        (list-ec (:list y ys)
                 (:list x xs)
                 (list x y zs))))))
  (let* ((front-top-left     '(-1  1  1))
         (front-top-right    '( 1  1  1))
         (front-bottom-left  '(-1 -1  1))
         (front-bottom-right '( 1 -1  1))
         (back-top-left      '(-1  1 -1))
         (back-top-right     '( 1  1 -1))
         (back-bottom-left   '(-1 -1 -1))
         (back-bottom-right  '( 1 -1 -1))
         (unit-cube (append! (gen-face front-top-left front-bottom-right) ; front
                            (gen-face front-top-right back-bottom-right) ; right
                            (gen-face back-top-right back-bottom-left)   ; back
                            (gen-face back-top-left front-bottom-left)   ; left
                            (gen-face back-top-left front-top-right)     ; top
                            (gen-face front-bottom-left back-bottom-right) ; bottom
                            ))
         (unit-sphere (append-ec (:list vert unit-cube)
                                 (let* ((x (first vert))
                                        (y (second vert))
                                        (z (third vert))
                                        (length (sqrt (+ (* x x) (* y y) (* z z))))
                                        (/length (/ length)))
                                   (list (* x /length) (* y /length) (* z /length)))))
         (n-verts-per-face (* resolution+1 resolution+1))
         (face-indices (append-ec (:range row resolution)
                                  (:range column resolution)
                                  (let* ((row (* row resolution+1))
                                         (next-row (+ row resolution+1))
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
         (indices (append-ec (:range i 6)
                             (let ((face (* i n-verts-per-face)))
                               (map (cut + face <>)
                                    face-indices))))
         (position (cons 'position (map (cut * radius <>)
                                        unit-sphere)))
         (normal (and normals? (cons 'normal unit-sphere)))
         (color (and color (cons 'color
                                 (flatten (list-tabulate (* 6 n-verts-per-face)
                                                         color))))) 
         (texture (cond
                   (texture (cons 'tex-coord
                                  (flatten (list-tabulate (* 6 n-verts-per-face)
                                                          texture))))
                   (cube-map?
                    (cons 'tex-coord unit-sphere))
                   (else #f))))
    (mesh-make (list position
                     normal
                     color
                     texture)
               indices
               winding
               index-type
               color-type
               texture-type
               3
               mode)))

(define (uv-sphere-mesh radius resolution normals? color mode winding
                        texture texture-width texture-height texture-offset
                        index-type color-type texture-type)
  (when (or (odd? resolution) (< resolution 4))
    (error 'uv-sphere-mesh "Resolution must be even and not less than 4:" resolution))
  (let* ((angle-increment (* 2 (/ pi resolution)))
         (n-lat-verts (add1 (/ resolution 2)))
         (resolution+1 (add1 resolution))
         (circle-points (map (lambda (angle)
                               (cons (- (cos angle)) (sin angle)))
                             (iota resolution+1 0 angle-increment)))
         (semi-circle-points (map (lambda (angle)
                                    (cons (cos angle)
                                          (sin angle)))
                                  (iota n-lat-verts 0 angle-increment)))
         (unit-sphere (append-ec (:list latitude semi-circle-points)
                                 (:list longitude circle-points)
                                 (list (* (car longitude) (cdr latitude))
                                       (car latitude)
                                       (* (cdr longitude) (cdr latitude)))))
         (indices (append-ec (:range lat (/ resolution 2))
                             (:range column resolution)
                             (let* ((row (* lat resolution+1))
                                    (next-row (+ row resolution+1))
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
         (position (cons 'position (map (cut * radius <>)
                                        unit-sphere)))
         (normal (and normals? (cons 'normal unit-sphere)))
         (color (and color (cons 'color
                                 (flatten (list-tabulate (* resolution+1
                                                            n-lat-verts)
                                                         color)))))
         (texture (build-texture texture texture-width texture-height texture-offset
                                 resolution+1 n-lat-verts)))
    (mesh-make (list position
                     normal
                     color
                     texture)
               indices
               winding
               index-type
               color-type
               texture-type
               2
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
  (let* ((angle-increment (* 2 (/ pi resolution)))
         (resolution+1 (add1 resolution))
         (circle-points (map (lambda (angle)
                               (cons (- (cos angle)) (sin angle)))
                             (iota resolution+1 0 angle-increment)))
         (vertices (append-ec (:list y (iota (add1 vertical-subdivisions)
                                             length
                                             (- (/ length vertical-subdivisions))))
                              (:list circle-point circle-points)
                                 (list (* (car circle-point) radius)
                                       y
                                       (* (cdr circle-point) radius))))
         (indices (append-ec (:range row vertical-subdivisions)
                             (:range column resolution)
                             (let* ((row (* row resolution+1))
                                    (next-row (+ row resolution+1))
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
         (normal-circle (append-ec (:list circle-point circle-points)
                                   (list (car circle-point)
                                         0
                                         (cdr circle-point))))
         (normal (and normals? (cons 'normal
                                     (append-ec (:range i (add1 vertical-subdivisions))
                                                normal-circle))))
         (color (and color
                     (cons 'color
                           (flatten (list-tabulate (* resolution+1
                                                      (add1 vertical-subdivisions))
                                                   color)))))
         (texture (build-texture texture texture-width texture-height texture-offset
                                 resolution+1 (add1 vertical-subdivisions))))
    (mesh-make (list position
                     normal
                     color
                     texture)
               indices
               winding
               index-type
               color-type
               texture-type
               2
               mode)))

) ; end module hypergiant-geometry
