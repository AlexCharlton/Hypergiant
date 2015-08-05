;;;; models.scm

;;;; IQM model loading and support for skeletal animations

(module hypergiant-models
(make-animated-model
 animated-model?
; add-new-animated-model
 %make-animated-model ;; TODO
 animate-model-frames
 update-animated-model!)

(import chicken scheme foreign)
(use hypergiant-sprites
     (prefix hyperscene scene:)
     (prefix gl-utils gl:) (except bitstring bitstring->vector) gl-math
     data-structures srfi-1 srfi-4 srfi-99 miscmacros lolevel)

(define-record-type (animated-model animated-sprite)
  %make-animated-model #t
  (current-frame) (n-joints))

;; (define (add-new-animated-sprite parent sprite-sheet texture base-animation)
;;   (let ((node (add-node parent sprite-pipeline-render-pipeline
;;                         mesh: sprite-sheet
;;                         tex: texture)))
;;     (make-animated-sprite node base-animation)))

(define (make-animated-model node base-animation n-joints)
  (let ((renderable (scene:node-data node)))
    (set-finalizer! (%make-animated-model node #f
                                          #f base-animation
                                          0 0.0
                                          (make-matrix-array n-joints)
                                          n-joints)
                    (lambda (m) (free (animated-model-current-frame m))))))

(define (animate-model-frames animation frame-matrices n-joints
                              frame next-frame frame-offset)
  (let* ((frames (animation-frames animation))
         (parents (cdr frames))
         (frames (car frames))
         (1-frame-offset (- 1 frame-offset)))
    (dotimes (i n-joints)
      (let* ((frame (nth-matrix frames (+ (* n-joints frame)
                                          i)))
             (next (nth-matrix frames (+ (* n-joints next-frame)
                                         i)))
             (m frame ;; (m+ (m*s frame 1-frame-offset)
                ;;     (m*s next frame-offset))
                )
             (parent (vector-ref parents i))
             (current-frame (nth-matrix frame-matrices i)))
        ;; need matrix addition, multiplication
        (if (>= parent 0)
            (m* (nth-matrix frame-matrices parent)
                m
                current-frame)
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
