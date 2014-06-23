(export random-normal
        clamp)

(define random-state (random-mtzig:init))

(define (random-normal #!optional mean variance)
  (+ mean
     (* (random-mtzig:randn! random-state)
        variance)))

(define (clamp x l u)
  (min (max x l) u))

