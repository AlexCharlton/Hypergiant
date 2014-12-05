(export random-normal
        random-float
        clamp
        vclamp
        vclamp!
        v/
        vround
        vceiling
        vtruncate
        vfloor)

(define random-state (random-mtzig:init))

(define (random-normal #!optional mean variance)
  (+ mean
     (* (random-mtzig:randn! random-state)
        variance)))

(define (random-float)
  (- (* 2 (f32vector-ref (random-mtzig:f32vector-randu! 1 random-state) 0))
     1))

(define (clamp x l u)
  (min (max x l) u))

(define (vclamp v l u)
  (make-point (clamp (point-x v) l u)
              (clamp (point-y v) l u)
              (clamp (point-z v) l u)))

(define (vclamp! v l u)
  (point-x-set! (clamp (point-x v) l u))
  (point-y-set! (clamp (point-y v) l u))
  (point-z-set! (clamp (point-z v) l u)))

(define (v/ v s)
  (v* v (/ s)))

(define (vround v)
  (make-point (round (point-x v))
              (round (point-y v))
              (round (point-z v))))

(define (vfloor v)
  (make-point (floor (point-x v))
              (floor (point-y v))
              (floor (point-z v))))

(define (vceiling v)
  (make-point (ceiling (point-x v))
              (ceiling (point-y v))
              (ceiling (point-z v))))

(define (vtruncate v)
  (make-point (truncate (point-x v))
              (truncate (point-y v))
              (truncate (point-z v))))
