;;;; models.scm

;;;; IQM model loading and support for skeletal animations

(module hypergiant-models
(make-animated-model
 animated-model?
 add-new-animated-model
 animate-model-frames
 update-animated-model!)

(import chicken scheme foreign)
(use hypergiant-sprites hypergiant-render-pipeline
     (prefix hyperscene scene:)
     (prefix gl-utils gl:) (except bitstring bitstring->vector) gl-math
     data-structures srfi-1 srfi-4 srfi-99 miscmacros lolevel)

(define-record-type (animated-model animated-sprite)
  %make-animated-model #t
  (current-frame) (n-joints))

(define (add-new-animated-model parent pipeline . args)
  (define (get-arg arg)
    (get-keyword arg args
                 (lambda () (error 'add-new-animated-model
                              (sprintf "Missing ~s keyword" arg)
                              args))))
  (let* ((model (get-arg model:))
         (mesh (get-arg mesh:))
         (texture (get-arg texture:))
         (base-animation (get-arg base-animation:))
         (n-joints (length (iqm-joints model)))
         (current-frame (make-matrix-array n-joints))
         (node (add-node parent pipeline
                         bone-matrices: current-frame
                         mesh: mesh
                         tex: texture))
         (animated-model (%make-animated-model node #f #f base-animation
                                               0 0.0
                                               current-frame n-joints)))
    (animate-model-frames base-animation current-frame n-joints 0 0 0)
    animated-model))

(define (make-animated-model node base-animation n-joints)
  (let ((renderable (scene:node-data node)))
    (set-finalizer! (%make-animated-model node #f
                                          #f base-animation
                                          0 0.0
                                          (make-matrix-array n-joints)
                                          n-joints)
                    (lambda (m) (free (animated-model-current-frame m))))))

(define m (allocate (* 16 4)))
(define t1 (allocate (* 16 4)))
(define t2 (allocate (* 16 4)))
(define (animate-model-frames animation frame-matrices n-joints
                              frame next-frame frame-offset)
  (let* ((frames (animation-frames animation))
         (parents (cdr frames))
         (frames (car frames)))
    (dotimes (i n-joints)
      (let* ((frame (nth-matrix frames (+ (* n-joints frame)
                                          i)))
             (next (nth-matrix frames (+ (* n-joints next-frame)
                                         i)))
             (parent (vector-ref parents i))
             (current-frame (nth-matrix frame-matrices i)))
        (m*s frame (- 1 frame-offset) t1)
        (m*s next frame-offset t2)
        (m+ t1 t2 m)
        (if (>= parent 0)
            (m* (nth-matrix frame-matrices parent) m current-frame)
            (copy-mat4 m current-frame))))))

(define (update-animated-model! model delta)
  (let* ((animation (current-animation model))
         (momentary? (animated-sprite-animation model))
         (frame-rate (animation-frame-rate animation))
         (n-frames (animation-n-frames animation))
         (frame (animated-sprite-frame model)) 
         (timer (+ (animated-sprite-timer model) delta)))
    (when (> timer frame-rate)
      (while (> timer frame-rate)
        (set! timer (- timer frame-rate))
        (if (and momentary? (= (add1 frame) n-frames))
            (begin
              (animated-sprite-animation-set! model #f)
              (animated-sprite-frame-set! model 0))
            (animated-sprite-frame-set! model (modulo (add1 frame)
                                                      n-frames))))
      (let* ((animation (current-animation model))
             (current-frames (animated-model-current-frame model))
             (frame (animated-sprite-frame model))
             (next-frame (modulo (add1 frame) n-frames))
             (frame-offset (/ timer frame-rate)))
        (animate-model-frames animation current-frames
                              (animated-model-n-joints model)
                              frame next-frame frame-offset)))
    (animated-sprite-timer-set! model timer)))

(include "iqm")

) ; end module hypergiant-models
