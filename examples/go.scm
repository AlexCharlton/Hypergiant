;;;; go.scm

;;;; This example implements the game of Go.
;;;; Click to place a stone, escape quits.

;; If these font paths are not on your system, substitute with a font that is.
(define font (cond-expand
	      (macosx "/Library/Fonts/Microsoft/Arial.ttf")
	      (else "/usr/share/fonts/truetype/msttcorefonts/arial.ttf")))

;;;; NOTE:
;;;; If this file is compiled, since it uses glls-render, it must also be linked with OpenGL
;;;; E.g.:
;;;; csc -lGL go.scm

(import chicken scheme)
(use hypergiant srfi-42 miscmacros)

;;;
;;; Game logic
;;;

;; Turns
(define turn (make-parameter 'black))

(define (next-turn)
  (turn (if (eq? (turn) 'black)
            'white
            'black)))


;; Nodes
(define grid-rows 19)

(define-record node
  index color scene-node)

(define (neighbours node)
  (let* ((index (node-index node))
         (x (car index))
         (y (cdr index))
         (north (and (< (add1 y) grid-rows) (get-node (cons x (add1 y)))))
         (south (and (>= (sub1 y) 0) (get-node (cons x (sub1 y)))))
         (west (and (>= (sub1 x) 0) (get-node (cons (sub1 x) y))))
         (east (and (< (add1 x) grid-rows) (get-node (cons (add1 x) y)))))
    (remove not (list north east south west))))

(define (find-chained-nodes node predicate)
  ;; Find nodes chained to the given one, that satisfy PREDICATE
  (let ((chain '()))
    (let find ((node node))
      (when (and (not (member node chain))
                 (predicate node))
        (set! chain (cons node chain))
        (map find (neighbours node))))
    chain))


;; Chains
(define-record chain
  color members liberties)

(define (get-chain node)
  (if* (find (lambda (chain) (find (cut equal? (node-index node) <>)
                              (chain-members chain)))
         (state-chains (game-state)))
       it
       (error 'get-chain "Tried to find node that is not in any chain" node)))

(define (add-to-chain chain node liberties)
  (chain-members-set! chain (cons (node-index node) (chain-members chain)))
  (chain-liberties-set! chain (append (map node-index liberties)
                                      (chain-liberties chain))))

(define (remove-from-chain chain node)
  (chain-members-set! chain (delete (node-index node) (chain-members chain)))
  (chain-liberties-set! chain (cons (node-index node) (chain-liberties chain))))

(define (delete-liberty chain node)
  (chain-liberties-set! chain (delete (node-index node) (chain-liberties chain))))

(define (add-liberty chain node)
  (chain-liberties-set! chain (cons (node-index node) (chain-liberties chain))))

(define (join-chains chainz)
  (let ((joined (make-chain (chain-color (car chainz))
                            (append-map chain-members chainz)
                            (append-map chain-liberties chainz))))
    (update-state-chains! (lambda (chains)
                            (cons joined
                                  (remove (cut member <> chainz) chains))))
    joined))

(define (new-chain color node open)
  (update-state-chains! (lambda (chains)
                          (cons (make-chain color
                                            (list (node-index node))
                                            (map node-index open))
                                chains))))

(define (update-chains-with-node node)
  (define (add-stone color)
    (receive (occupied open)
        (partition node-color (neighbours node))
      (receive (friendlies enemies)
          (partition (lambda (n)
                       (equal? (node-color n)
                               color))
                     occupied)
        (if (null? friendlies)
            (new-chain color node open)
            (let ((chains (delete-duplicates (map get-chain friendlies))))
              (add-to-chain (if (> (length chains) 1)
                                (join-chains chains)
                                (car chains))
                            node open)))
        (for-each (cut delete-liberty <> node)
                  (delete-duplicates (map get-chain occupied))))))
  (define (remove-stone node)
    (let ((chain (get-chain node)))
      (remove-from-chain chain node)
      (when (null? (chain-members chain))
        (update-state-chains! (lambda (chains)
                                (delete chain chains eq?))))
      (let ((neighbouring-chains
             (delete-duplicates
              (remove not
                      (map (lambda (node)
                             (and (node-color node)
                                  (not (member (node-index node)
                                               (chain-members chain)))
                                  (get-chain node)))
                           (neighbours node))))))
        (map (cut add-liberty <> node) neighbouring-chains))))
  (if* (node-color node)
       (add-stone it)
       (remove-stone node)))

(define (delete-chain chain)
  (for-each delete-stone 
            (map get-node (chain-members chain))))

(define (check-for-dead-chains color)
  (define (suicide-exn chain)
    (make-property-condition 'game-logic 'suicide chain))
  (receive (friendly-chains enemy-chains)
      (partition (lambda (chain)
                   (eq? (chain-color chain)
                        color))
                 (state-chains (game-state)))
    (for-each (lambda (chain)
                (when (null? (chain-liberties chain))
                  (delete-chain chain)))
              enemy-chains)
    (for-each (lambda (chain)
                (when (null? (chain-liberties chain))
                  (signal (suicide-exn chain))))
              friendly-chains)))


;; Game state
(define-record state
  nodes chains)

(define game-state
  (make-parameter
   (make-state (list-ec (: i grid-rows)
                        (: j grid-rows)
                        (make-node (cons j i) #f #f))
               '())))

(define (copy-state state)
  (define (copy-node node)
    (make-node (node-index node)
               (node-color node)
               (node-scene-node node)))
  (define (copy-chain chain)
    (make-chain (chain-color chain)
                (chain-members chain)
                (chain-liberties chain)))
  (make-state (map copy-node (state-nodes state))
              (map copy-chain (state-chains state))))

(define (compress-state)
  (map node-color (state-nodes (game-state))))

(define (update-state-chains! fun)
  (state-chains-set! (game-state) (fun (state-chains (game-state)))))

(define (get-node index)
  (list-ref (state-nodes (game-state))
            (+ (car index)
               (* (cdr index)
                  grid-rows))))


;; History
(define game-history (make-parameter '()))
(define history-check-limit 20)

(define (check-for-repeated-state)
  (let ((compressed (compress-state))
        (recent-history (if (> (length (game-history))
                               history-check-limit)
                            (take (game-history) history-check-limit)
                            (game-history))))
    (when (member compressed recent-history)
      (signal (make-property-condition 'game-logic 'repeated compressed)))
    (game-history (cons compressed (game-history)))))


;; Stones
(define (delete-stone node)
  (node-color-set! node #f)
  (update-chains-with-node node))

(define (add-stone node color)
  (when (node-color node)
    (signal (make-property-condition 'game-logic 'occupied node)))
  (node-color-set! node color)
  (update-chains-with-node node))

(define (place-stone index)
  (let ((color (turn))
        (new-state (copy-state (game-state))))
    (game-state
     (let ((old-state (game-state)))
       (parameterize ((game-state new-state))
         (let ((node (get-node index)))
           (condition-case
               (begin
                 (add-stone node color)
                 (check-for-dead-chains color)
                 (check-for-repeated-state)
                 (update-scene old-state (game-state))
                 (next-turn)
                 (game-state))
             ((game-logic) old-state))))))))


;; Scoring
(define (get-score)
  (define empty-chains (make-parameter '()))
  (define (get-chain node)
    (find (lambda (chain) (find (cut equal? (node-index node) <>)
                           (chain-members chain)))
          (empty-chains)))
  (define (add-node-to-empty-chains node)
    (unless (get-chain node)
      (let* ((color #f)
             (nodes (find-chained-nodes
                     node
                     (lambda (node)
                       (if* (node-color node)
                            (begin
                              (unless (eq? color 'none)
                                (if color
                                    (when (not (eq? color it))
                                      (set! color 'none))
                                    (set! color it)))
                                   #f)
                            #t)))))
        (empty-chains (cons (make-chain color (map node-index nodes) #f)
                           (empty-chains))))))
  (let ((score (map (cut cons <> 0) '(black white))))
    (for-each (lambda (node)
                (if* (node-color node)
                     (alist-update! it
                                    (add1 (alist-ref it score))
                                    score)
                     (add-node-to-empty-chains node)))
              (state-nodes (game-state)))
    (for-each (lambda (chain)
                (if* (chain-color chain)
                     (when (not (eq? it 'none))
                       (alist-update! it
                                      (+ (length (chain-members chain))
                                         (alist-ref it score))
                                      score))))
              (empty-chains))
    (for-each (lambda (color)
                (if (= (alist-ref color score) 361)
                    (alist-update! color 1 score)))
              '(black white))
    score))


;;;
;;; Scene and graphics
;;;

(define scene (make-parameter #f))
(define camera (make-parameter #f))

(define (update-scene old-state new-state)
  (for-each (lambda (old-node new-node)
              (let ((old-stone (node-color old-node))
                    (new-stone (node-color new-node)))
                (unless (eq? old-stone new-stone)
                  (when old-stone
                    (delete-node (node-scene-node old-node)))
                  (when new-stone
                    (add-stone-to-scene new-node)))))
            (state-nodes old-state)
            (state-nodes new-state)))
;; Pipelines
(define-pipeline phong-pipeline 
  ((#:vertex input: ((position #:vec3) (normal #:vec3))
             uniform: ((mvp #:mat4) (model #:mat4))
             output: ((p #:vec3) (n #:vec3)))
   (define (main) #:void
     (set! gl:position (* mvp (vec4 position 1.0)))
     (set! p (vec3 (* model (vec4 position 1))))
     (set! n normal)))
  ((#:fragment input: ((n #:vec3) (p #:vec3))
               use: (phong-lighting)
               uniform: ((color #:vec3)
                         (inverse-transpose-model #:mat4)
                         (camera-position #:vec3)
                         (ambient #:vec3)
                         (n-lights #:int)
                         (light-positions (#:array #:vec3 8))
                         (light-colors (#:array #:vec3 8))
                         (light-intensities (#:array #:float 8))
                         (material #:vec4))
               output: ((frag-color #:vec4)))
   (define (main) #:void
     (set! frag-color
       (light (vec4 color 1) p n)))))

(define-pipeline wood-pipeline
  ((#:vertex input: ((position #:vec3) (normal #:vec3))
             uniform: ((mvp #:mat4) (model #:mat4))
             output: ((p #:vec3) (n #:vec3)))
   (define (main) #:void
     (set! gl:position (* mvp (vec4 position 1.0)))
     (set! p (vec3 (* model (vec4 position 1))))
     (set! n normal)))
  ((#:fragment input: ((p #:vec3) (n #:vec3))
               uniform: ((color #:vec3)
                         (camera-position #:vec3)
                         (inverse-transpose-model #:mat4)
                         (ambient #:vec3)
                         (n-lights #:int)
                         (light-positions (#:array #:vec3 8))
                         (light-colors (#:array #:vec3 8))
                         (light-intensities (#:array #:float 8))
                         (material #:vec4))
               use: (simplex-noise-3d phong-lighting)
               output: ((frag-color #:vec4)))
   (define (grain (resolution #:int)) #:vec4
     (let ((n #:float (snoise (* p
                                 (vec3 1 8 1)
                                 resolution))))
       (vec4 (+ color (* n 0.1)) 1)))
   (define (main) #:void
     (set! frag-color (light (* (grain 32) (grain 4)) p n)))))

;; Meshes and nodes
(define board-mesh (cube-mesh 1 normals?: #t))
(mesh-transform! 'position board-mesh
                 (3d-scaling 1.2 1.2 0.06))

(define line-width (/ 256))
(define grid-line (rectangle-mesh (+ 1 line-width) line-width
                                  centered?: #f))

(define (build-grid)
  (let* ((-line-width/2 (- (/ line-width 2)))
         (line-spacing (/ (sub1 grid-rows)))
         (lateral-lines
          (let loop ((i 0) (lines '()))
            (if (= i grid-rows)
                lines
                (loop (add1 i)
                      (cons
                       (cons grid-line
                             (translation
                              (make-point -line-width/2
                                          (+ (* i line-spacing)
                                             -line-width/2)
                                          0)))
                       lines)))))
         (vertical-lines
          (map (lambda (a)
                 (cons grid-line
                       (translate (make-point 0 1 0)
                                  (rotate-z (- pi/2)
                                            (copy-mat4 (cdr a))))))
                 lateral-lines)))
    (append lateral-lines
            vertical-lines)))

(define marker (circle-mesh (/ 120) 12))

(define (build-markers)
  (let* ((3nodes (/ 3 (sub1 grid-rows)))
         (15nodes (/ 15 (sub1 grid-rows)))
         (marker-points `((,3nodes  . ,3nodes)
                          (,3nodes  . 0.5)
                          (,3nodes  . ,15nodes)
                          (0.5      . ,3nodes)
                          (0.5      . 0.5)
                          (0.5      . ,15nodes)
                          (,15nodes . ,3nodes)
                          (,15nodes . 0.5)
                          (,15nodes . ,15nodes))))
    (map (lambda (p)
           (cons marker
                 (translation (make-point (car p) (cdr p) 0))))
         marker-points)))

(define board-grid-mesh (mesh-transform-append
                         'position
                         (append (build-grid)
                                 (build-markers))))

(define brown (make-rgb-color 0.5 0.4 0.2 #t))
(define shiny-material (make-material 0.5 0.5 0.5 10))

(define (init-board)
  (add-node (scene) wood-pipeline-render-pipeline
            mesh: board-mesh
            color: brown
            material: shiny-material
            position: (make-point 0.5 0.5 -0.03))
  (add-node (scene) mesh-pipeline-render-pipeline
            mesh: board-grid-mesh
            color: black
            position: (make-point 0 0 0.0003)))

(define stone-radius (/ 40))
(define stone-half-height 0.4)
(define stone-mesh (sphere-mesh stone-radius 16 normals?: #t))
(mesh-transform! 'position stone-mesh
                 (3d-scaling 1 1 stone-half-height))
(define colors `((white . ,white)
                 (black . ,black)))

(define (add-stone-to-scene node)
  (let* ((index (node-index node))
         (n (add-node (scene) phong-pipeline-render-pipeline
                      mesh: stone-mesh
                      color: (alist-ref (node-color node) colors)
                      material: shiny-material
                      position: (make-point (/ (car index)
                                               (sub1 grid-rows))
                                            (/ (cdr index)
                                               (sub1 grid-rows))
                                            (* stone-radius
                                               stone-half-height))
                      radius: stone-radius)))
    (node-scene-node-set! node n)))

;; Score
(define score-face (make-parameter #f))
(define score-node (make-parameter #f))
(define score-mesh (make-parameter #f))

(define (init-score)
  (score-face (load-face font 20))
  (score-mesh (string-mesh "Black: 000  White: 000" (score-face)))
  (score-node (add-node ui text-pipeline-render-pipeline
                        tex: (face-atlas (score-face))
                        color: black
                        position: (make-point 10 -10 0)
                        mesh: (score-mesh)
                        usage: #:dynamic))
  (update-score))

(define (update-score)
  (let* ((s (get-score))
         (black-score
          (string-append "Black: "
                         (number->string (alist-ref 'black s))))
         (white-score
          (string-append "White: "
                         (number->string (alist-ref 'white s))))
         (score (string-append black-score "  " white-score)))
    (update-string-mesh! (score-mesh) (score-node) score (score-face))))


;;;
;;; Input and main loop
;;;
(define keys (make-bindings
              `((quit ,+key-escape+ press: ,stop))))

(define (get-cursor-board-position)
  (receive (near far) (get-cursor-world-position (camera))
    (let ((u (/ (point-z near) (- (point-z near) (point-z far)))))
      (make-point (+ (point-x near) (* u (- (point-x far)
                                            (point-x near))))
                  (+ (point-y near) (* u (- (point-y far)
                                            (point-y near))))
                  0))))

(define (get-nearest-index)
  (let ((n (vround (v* (vclamp (get-cursor-board-position) 0 1)
                       (sub1 grid-rows)))))
    (cons (inexact->exact (point-x n))
                    (inexact->exact (point-y n)))))

(define (cursor-board-press)
  (place-stone (get-nearest-index))
  (update-score))

(define mouse (make-bindings
               `((left-click ,+mouse-button-left+
                  press: ,cursor-board-press))))

(define (init)
  (push-key-bindings keys)
  (push-mouse-bindings mouse)
  (scene (make-scene))
  (activate-extension (scene) (lighting))
  (set-ambient-light! (scene) (make-rgb-color 0.4 0.4 0.4))
  (add-light (scene) white 100 position: (make-point 0 0 2))
  (camera (make-camera #:perspective #:position (scene)
                       near: 0.001 angle: 35))
  (set-camera-position! (camera) (make-point 0.5 0.5 2))
  (set-camera-position! (camera) (make-point 1.6 -0.6 1.3))
  (quaternion-rotate-z
   (degrees->radians 45) (quaternion-rotate-x
                          (degrees->radians 45)
                          (camera-rotation (camera))))
  (gl:clear-color 0.8 0.8 0.8 1)
  (init-board)
  (init-score))

(start 800 600 "Go" resizable: #f init: init)
