;;;; utils.scm

;;;; Small functions that don't have a home elsewhere
;;;; Imported by hypergiant.scm

(export update-string-mesh!
        make-string-mesh
        make-rgb-color
        make-rgba-color
        color-r color-g color-b color-a
        color-r-set! color-g-set! color-b-set! color-a-set!
        black white
        add-light)

(use srfi-4 miscmacros)

(define (update-string-mesh! mesh node string face)
  (if (and node (zero? (string-length string)))
      (glls:set-renderable-n-elements! (scene:node-data node) 0)
      (begin
        (string-mesh string face mesh: mesh)
        (when node
          (glls:set-renderable-n-elements! (scene:node-data node)
                                           (mesh-n-indices mesh))))))

(define (make-string-mesh n-chars)
  (make-mesh vertices: `(attributes: ((position #:short 2)
                                      (tex-coord #:unsigned-short 2
                                                 normalized: #t))
                         n-vertices: ,(* n-chars 4))
             indices: `(type: #:ushort
                        n-indices: ,(* n-chars 6))))

;;; Colors
(define (make-rgb-color r g b #!optional non-gc?)
  (let ((v (make-f32vector 3 0 non-gc?)))
    (f32vector-set! v 0 r)
    (f32vector-set! v 1 g)
    (f32vector-set! v 2 b)
    v))

(define (make-rgba-color r g b a #!optional non-gc?)
  (let ((v (make-f32vector 4 0 non-gc?)))
    (f32vector-set! v 0 r)
    (f32vector-set! v 1 g)
    (f32vector-set! v 2 b)
    (f32vector-set! v 3 a)
    v))

(define (color-r c)
  (f32vector-ref c 0))

(define (color-g c)
  (f32vector-ref c 1))

(define (color-b c)
  (f32vector-ref c 2))

(define (color-a c)
  (f32vector-ref c 3))

(define (color-r-set! c r)
  (f32vector-set! c 0 r))

(define (color-g-set! c g)
  (f32vector-set! c 1 g))

(define (color-b-set! c b)
  (f32vector-set! c 2 b))

(define (color-a-set! c a)
  (f32vector-set! c 3 a))

(define black (make-rgb-color 0 0 0 #t))
(define white (make-rgb-color 1 1 1 #t))

;;; Hyperscene
(define (add-light node color intensity . args)
  (let ((light (apply scene:add-light node color intensity args)))
    (if* (get-keyword position: args)
         (scene:set-node-position! light it))
    (if* (get-keyword radius: args)
         (scene:set-node-bounding-sphere! light it))
    light))
