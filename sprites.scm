;;;; sprites.scm

;;;; Implementation of animated sprites

(module hypergiant-sprites
(make-sprite-sheet
 make-animation
 make-animation-alist
 set-animation!
 current-animation
 make-animated-sprite
 add-new-animated-sprite
 update-animated-sprite!
 animated-sprite?
 animated-sprite-node
 animation?)

(import chicken scheme)
(use (prefix glls-render glls:) (prefix hyperscene scene:) gl-utils
     hypergiant-render-pipeline hypergiant-shaders
     srfi-1 srfi-42 srfi-99 miscmacros)

(define-record-type animated-sprite
  %make-animated-sprite #t
  (node) (renderable) (animation) (base-animation)
  (frame) (timer))

(define-record-type animation
  %make-animation #t
  (type) (frames) (n-frames) (frame-rate))

(define (make-sprite-sheet tex-w tex-h frame-w frame-h
                           #!key rows columns (x-offset 0) (y-offset 0)
                           (centered? #t) (texture-type #:ushort))
  (let ((rows (or rows (quotient (- tex-h y-offset) frame-h)))
        (columns (or columns (quotient (- tex-w x-offset) frame-w))))

    (when (> (+ y-offset (* frame-h rows)) tex-h)
      (error 'make-sprite-sheet "Invalid sprite-sheet specification, texture too short"))
    (when (> (+ x-offset (* frame-w columns)) tex-w)
      (error 'make-sprite-sheet "Invalid sprite-sheet specification, texture too narrow"))
    (let* ((pos (if centered?
                    (let* ((x2 (* frame-w 0.5))
                           (y2 (* frame-h 0.5))
                           (x (- x2))
                           (y (- y2)))
                      (circular-list x  y2 0
                                     x2 y2 0
                                     x  y  0
                                     x2 y  0))
                    (circular-list 0       frame-h 0
                                   frame-w frame-h 0
                                   0       0       0
                                   frame-w 0       0)))
           (position (take pos (* 12 rows columns)))
           (tex-coord (append-ec (:range i rows)
                                 (:range j columns)
                                 (let ((top (/ (+ y-offset (* frame-h i))
                                               tex-h))
                                       (bottom (/ (+ y-offset (* frame-h (add1 i)))
                                                  tex-h))
                                       (left (/ (+ x-offset (* frame-w j))
                                                tex-w))
                                       (right (/ (+ x-offset (* frame-w (add1 j)))
                                                 tex-w)))
                                   (list left top right top 
                                         left bottom right bottom))))
           (indices (append-ec (:range i (* rows columns))
                               (let ((i (* 4 i)))
                                 (list (+ 0 i) (+ 2 i) (+ 1 i)
                                       (+ 1 i) (+ 2 i) (+ 3 i))))))
      (make-mesh vertices: `(attributes: ((position #:float 3)
                                          (tex-coord ,texture-type 2 normalized: #t))
                                         initial-elements: ((position . ,position)
                                                            (tex-coord . ,tex-coord)))
                 indices: `(type: #:ushort
                                  initial-elements: ,indices)))))

(define (make-animated-sprite node base-animation)
  (when (eq? (animation-type base-animation) #:once)
    (error 'make-animated-sprite "Only looping animations may be set as the base animation" base-animation))
  (let ((renderable (scene:node-data node)))
    (glls:set-renderable-n-elements! renderable 6)
    (glls:set-renderable-offset! renderable
                                 (* 12 (car (animation-frames base-animation))))
    (%make-animated-sprite node renderable #f base-animation
                           0 0.0)))

(define (add-new-animated-sprite parent sprite-sheet texture base-animation)
  (let ((node (add-node parent sprite-pipeline-render-pipeline
                        mesh: sprite-sheet
                        tex: texture)))
    (make-animated-sprite node base-animation)))

(define (make-animation type frames frame-rate)
  (%make-animation type frames (length frames) frame-rate))

(define (make-animation-alist animations #!key frame-rate)
  (map (lambda (a)
         (let ((type (second a))
               (frames (third a))
               (name (first a))
               (frame-rate (if (= (length a) 3)
                               (if frame-rate
                                   frame-rate
                                   (error 'make-animation-alist
                                          "Either animations include a frame-rate, or a default frame-rate must be specified"))
                               (fourth a))))
           (cons name (make-animation type frames frame-rate))))
       animations))

(define (set-animation! sprite animation)
  (let ((durration (if (eq? (animation-type animation) #:once)
                       #:momentary
                       #:base)))
    (unless (or (and (eq? durration #:base)
                     (eq? animation (animated-sprite-base-animation sprite)))
                (and (eq? durration #:momentary)
                     (eq? animation (animated-sprite-animation sprite))))
      (if (eq? durration #:base)
          (begin
            (animated-sprite-base-animation-set! sprite animation)
            (animated-sprite-animation-set! sprite #f))
          (animated-sprite-animation-set! sprite animation))
      (animated-sprite-timer-set! sprite 0.0)
      (animated-sprite-frame-set! sprite 0))))

(define (current-animation sprite)
  (if* (animated-sprite-animation sprite)
       it
       (animated-sprite-base-animation sprite)))

(define (update-animated-sprite! sprite delta)
  (let* ((animation (current-animation sprite))
         (momentary? (animated-sprite-animation sprite))
         (frame-rate (animation-frame-rate animation))
         (n-frames (animation-n-frames animation))
         (frame (animated-sprite-frame sprite)) 
         (timer (+ (animated-sprite-timer sprite) delta)))
    (if (> timer frame-rate)
        (begin
          (if (and momentary? (= (add1 frame) n-frames))
              (begin
                (animated-sprite-animation-set! sprite #f)
                (animated-sprite-timer-set! sprite 0.0)
                (animated-sprite-frame-set! sprite 0))
              (begin
                (animated-sprite-timer-set! sprite (- timer frame-rate))
                (animated-sprite-frame-set! sprite (if (= (add1 frame) n-frames)
                                                       0
                                                       (add1 frame)))))
          (let ((frame (list-ref (animation-frames (current-animation sprite))
                                 (animated-sprite-frame sprite))))
            (glls:set-renderable-offset! (animated-sprite-renderable sprite)
                                         (* frame 12))))
        (animated-sprite-timer-set! sprite timer))))

) ; end module hypergiant-sprites
