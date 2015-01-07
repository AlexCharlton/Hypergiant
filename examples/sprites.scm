;;;; sprites.scm

;;;; This example illustrates the use of animated sprites
;;;; This must be run in the examples directory

;;;; Left and right to run, space jumps

;; If these font paths are not on your system, substitute with a font that is.
(define font (cond-expand
	      (macosx "/Library/Fonts/Microsoft/Arial.ttf")
	      (else "/usr/share/fonts/truetype/msttcorefonts/arial.ttf")))

(import chicken scheme)
(use hypergiant data-structures)

(define scene (make-parameter #f))
(define camera (make-parameter #f))

(define frame-rate-font (make-parameter #f))
(define frame-rate-mesh (make-parameter #f))
(define frame-rate-node (make-parameter #f))

(define player-sprite (make-parameter #f))
(define player-sprite-sheet (make-sprite-sheet 256 256 32 32 rows: 6))
(m*vector-array! (scaling 2) (mesh-vertex-data player-sprite-sheet)
                 stride: (mesh-stride player-sprite-sheet))
(define animations (make-animation-alist 
                    `((stand #:loop ,(iota 4))
                      (fall  #:loop ,(iota 4 4))
                      (run   #:loop ,(iota 8 8))
                      (jump  #:once ,(iota 4 16))
                      (land  #:once ,(iota 4 24)))
                    frame-rate: 0.1))
(define player (make-parameter #f))

(define ground-mesh (rectangle-mesh 800 200
                                    color: (lambda (_) (list 0.4 0.3 0.2))))
(define sun-mesh (circle-mesh 50 24
                              color: (lambda (_) (list 0.9 0.9 0.3))))

(define jump-velocity (make-parameter #f))
(define jump-height 20)
(define run-dir (make-parameter 0))

(define (jump)
  (unless (jump-velocity)
    (jump-velocity jump-height)
    (set-animation! (player) (alist-ref 'fall animations))
    (set-animation! (player) (alist-ref 'jump animations))))

(define (face-right)
  (quaternion-y-rotation 0 (node-rotation (animated-sprite-node (player))))
  (node-needs-update! (animated-sprite-node (player))))

(define (face-left)
  (quaternion-y-rotation pi (node-rotation (animated-sprite-node (player))))
  (node-needs-update! (animated-sprite-node (player))))

(define (update-player-state)
  (cond
   ((positive? (run-dir)) (face-right))
   ((negative? (run-dir)) (face-left)))
  (when (jump-velocity)
    (move-node! (animated-sprite-node (player)) (make-point 0 (jump-velocity) 0))
    (if (= (jump-velocity) (- jump-height))
        (jump-velocity #f)
        (jump-velocity (sub1 (jump-velocity)))))
  (unless (jump-velocity)
    (if (= (run-dir) 0)
        (set-animation! (player) (alist-ref 'stand animations))
        (set-animation! (player) (alist-ref 'run animations)))))

(define (add-frame-rate)
  (frame-rate-mesh (string-mesh "...." (frame-rate-font)))
  (frame-rate-node
   (add-node ui text-pipeline-render-pipeline
             mesh: (frame-rate-mesh)
             color: black
             tex: (face-atlas (frame-rate-font))
             position: (make-point 550 -10 0)
             usage: #:stream)))

(define (update-frame-rate)
  (let ((fr (number->string (inexact->exact (round (frame-rate))))))
    (update-string-mesh! (frame-rate-mesh) (frame-rate-node)
                         fr
                         (frame-rate-font))))

(define (update delta)
  (update-player-state)
  (update-animated-sprite! (player) delta)
  (update-frame-rate))

(define keys (make-bindings
              `((quit ,+key-escape+ press: ,stop)
                (right ,+key-right+ toggle: ,run-dir)
                (left ,+key-left+ reverse-toggle: ,run-dir)
                (jump ,+key-space+ press: ,jump))))

(define (init)
  (gl:clear-color 0.6 0.8 1.0 1.0)
  (push-key-bindings keys)
  (scene (make-scene))
  (camera (make-camera #:ortho #:position (scene)))
  (set-camera-position! (camera) (make-point 0 100 1))
  (frame-rate-font (load-face font 20))
  (add-frame-rate)
  (player-sprite (load-ogl-texture "playersheet.dds" 0 0 0))
  (gl:set-texture-properties (player-sprite) mag: gl:+nearest+)
  (player (add-new-animated-sprite (scene) player-sprite-sheet
                                   (player-sprite)
                                   (alist-ref 'stand animations)))
  (add-node (scene) color-pipeline-render-pipeline
            mesh: ground-mesh
            position: (make-point 0 -130 -2))
  (add-node (scene) color-pipeline-render-pipeline
            mesh: sun-mesh
            position: (make-point -200 260 0)))

(start 640 480 "Sprites" resizable: #f init: init update: update)
