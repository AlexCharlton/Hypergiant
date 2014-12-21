;;;; go.scm

;;;; This example implements the game of Go.
;;;; Click to place a stone, escape quits.

;; If /usr/share/fonts/truetype/msttcorefonts/arial.ttf is not on your system, substitute with a font that is.
(define font "/usr/share/fonts/truetype/msttcorefonts/arial.ttf")

;;;; NOTE:
;;;; If this file is compiled, since it uses glls-render, it must also be linked with OpenGL
;;;; E.g.:
;;;; csc -lGL go.scm

(import chicken scheme)
(use hypergiant miscmacros srfi-42 srfi-99 data-structures srfi-1 srfi-69)

;;;
;;; Rules
;;;

;; Turns
(define stone-colors (make-parameter '(black white)))
(define turn (make-parameter 'black))

(define (next-turn)
  (turn (let ((rst (cdr (member (turn) (stone-colors)))))
          (if (null? rst)
              (car (stone-colors))
              (car rst)))))

;; Nodes
(define n-nodes (make-parameter 19))

(define-record-type node
  #t #t
  index
  point
  (stone))

(define-record-printer (node n out)
  (fprintf out "#,(node ~S ~S)"
    (node-index n)
    (if* (node-stone n)
         (stone-color it)
         #f)))

(define (get-node index)
  (list-ref (state-nodes (game-state))
            (+ (car index)
               (* (cdr index)
                  (n-nodes)))))

(define (neighbours node)
  (let* ((index (node-index node))
         (x (car index))
         (y (cdr index))
         (north (and (< (add1 y) (n-nodes)) (get-node (cons x (add1 y)))))
         (south (and (>= (sub1 y) 0) (get-node (cons x (sub1 y)))))
         (west (and (>= (sub1 x) 0) (get-node (cons (sub1 x) y))))
         (east (and (< (add1 x) (n-nodes)) (get-node (cons (add1 x) y)))))
    (remove not (list north east south west))))

(define (find-chained-nodes node predicate)
  (let ((chain '()))
    (let find ((node node))
      (when (and (not (member node chain))
               (predicate node))
        (set! chain (cons node chain))
        (map find (neighbours node))))
    chain))

;; State
(define game-state (make-parameter #f))

(define-record-type state
  #t #t
  (nodes) (chains))

(define-record-printer (state s out)
  (fprintf out "#,(state ~S ~S)"
    (state-nodes s)
    (state-chains s)))

(define (copy-state state)
  (define (copy-node node)
    (make-node (node-index node)
               (node-point node)
               (node-stone node)))
  (define (copy-chain chain)
    (make-chain (chain-color chain)
                (chain-members chain)
                (chain-liberties chain)))
  (make-state (map copy-node (state-nodes state))
              (map copy-chain (state-chains state))))

(define (compress-state)
  (map (lambda (node)
         (if* (node-stone node)
              (stone-color it)
              #f))
       (state-nodes (game-state))))

(define (generate-state)
  (let ((nodes (list-ec (: i (n-nodes))
                        (: j (n-nodes))
                        (make-node (cons j i)
                                   (make-point (/ j (sub1 (n-nodes)))
                                               (/ i (sub1 (n-nodes)))
                                               0)
                                   #f))))
    (game-state (make-state nodes '()))))

(define (update-state-chains! fun)
  (state-chains-set! (game-state) (fun (state-chains (game-state)))))

;; Chains
(define-record-type chain
  #t #t
  color
  (members)
  (liberties))

(define-record-printer (chain c out)
  (fprintf out "#,(chain ~S ~S ~S)"
    (chain-color c) (chain-members c) (chain-liberties c)))


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
  (define (add-stone stone)
    (let ((color (stone-color stone)))
      (receive (occupied open) (partition node-stone (neighbours node))
        (receive (friendlies enemies) (partition (lambda (n)
                                                   (equal? (stone-color
                                                            (node-stone n))
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
                    (delete-duplicates (map get-chain occupied)))))))
  (define (remove-stone node)
    (let ((chain (get-chain node)))
      (remove-from-chain chain node)
      (when (null? (chain-members chain))
        (update-state-chains! (lambda (chains)
                                (delete chain chains eq?))))
      (let ((neighbouring-chains (delete-duplicates
                                  (remove not
                                          (map (lambda (node)
                                                 (and (node-stone node)
                                                    (not (member (node-index node)
                                                               (chain-members chain)))
                                                    (get-chain node)))
                                               (neighbours node))))))
        (map (cut add-liberty <> node) neighbouring-chains))))
  (if* (node-stone node)
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

;; Stones
(define-record-type stone
  #t #t
  color
  (scene-node))

(define (delete-stone node)
  (node-stone-set! node #f)
  (update-chains-with-node node))

(define (add-stone node color)
  (when (node-stone node)
    (signal (make-property-condition 'game-logic 'occupied node)))
  (node-stone-set! node (make-stone color #f))
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


(define (update-scene old-state new-state)
  (for-each (lambda (old-node new-node)
              (let ((old-stone (node-stone old-node))
                    (new-stone (node-stone new-node)))
                (unless (eq? old-stone new-stone)
                  (when old-stone
                    (delete-node (stone-scene-node old-stone)))
                  (when new-stone
                    (add-stone-to-scene new-stone (node-point new-node))))))
            (state-nodes old-state)
            (state-nodes new-state)))

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
                       (if* (node-stone node)
                            (begin
                              (unless (eq? color 'none)
                                (if color
                                    (when (not (eq? color (stone-color it)))
                                      (set! color 'none))
                                    (set! color (stone-color it))))
                                   #f)
                            #t)))))
        (empty-chains (cons (make-chain color (map node-index nodes) #f)
                           (empty-chains))))))
  (let ((score (map (cut cons <> 0) (stone-colors))))
    (for-each (lambda (node)
                (if* (node-stone node)
                     (let ((color (stone-color it)))
                       (alist-update! color
                                      (add1 (alist-ref color score))
                                      score))
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
              (stone-colors))
    score))


;;;
;;; Rendering
;;;
(define scene (make-parameter #f))
(define camera (make-parameter #f))
(define lines (make-parameter #f))
(define score (make-parameter #f))
(define score-mesh (make-parameter #f))
(define shiny-material (make-material 0.5 0.5 0.5 10))
(define colors `((white . ,(f32vector 1 1 1))
                 (black . ,(f32vector 0 0 0))))
(define stone-radius (/ 40))
(define stone-mesh (sphere-mesh stone-radius 16 normals?: #t))
(m*vector-array! (3d-scaling 1 1 0.4) (mesh-vertex-data stone-mesh)
                 stride: (mesh-stride stone-mesh))
(define board-mesh (cube-mesh 1 normals?: #t))
(m*vector-array! (3d-scaling 1.2 1.2 0.06) (mesh-vertex-data board-mesh)
                 stride: (mesh-stride board-mesh))
(define marker (circle-mesh (/ 120) 12))

;; Shaders
(define-pipeline board-shader
  ((#:vertex input: ((position #:vec3) (normal #:vec3))
             uniform: ((mvp #:mat4) (model #:mat4))
             output: ((p #:vec3) (n #:vec3)))
   (define (main) #:void
     (set! p position)
     (set! n normal)
     (set! gl:position (* mvp (vec4 position 1.0)))))
  ((#:fragment input: ((p #:vec3) (n #:vec3))
               uniform: ((camera-position #:vec3)
                         (inverse-transpose-model #:mat4)
                         (ambient #:vec3)
                         (n-lights #:int)
                         (light-positions (#:array #:vec3 8))
                         (light-colors (#:array #:vec3 8))
                         (light-intensities (#:array #:float 8))
                         (material #:vec4))
               use: (simplex-noise-3d phong-lighting)
               output: ((frag-color #:vec4)))
   (define (fine) #:vec4
     (let ((n #:float (snoise (vec3 (* p.x 32) (* p.y 128) (* p.z 32)))))
       (vec4 (+ (vec3 0.5 0.4 0.2) (* n 0.1)) 1)))
   (define (course) #:vec4
     (let ((n #:float (snoise (vec3 (* p.x 4) (* p.y 32) (* p.z 4)))))
       (vec4 (+ (vec3 0.5 0.4 0.2) (* n 0.1)) 1)))
   (define (main) #:void
     (set! frag-color (light (* (fine) (course)) p
                             (normalize (* (mat3 inverse-transpose-model) n)))))))

(define-pipeline phong-pipeline 
  ((#:vertex input: ((position #:vec3) (normal #:vec3))
             uniform: ((mvp #:mat4) (model #:mat4))
             output: ((p #:vec3) (n #:vec3) (t #:vec2)))
   (define (main) #:void
     (set! gl:position (* mvp (vec4 position 1.0)))
     (set! p (vec3 (* model (vec4 position 1))))
     (set! n normal)))
  ((#:fragment input: ((n #:vec3) (p #:vec3))
               use: (phong-lighting)
               uniform: ((camera-position #:vec3)
                         (inverse-transpose-model #:mat4)
                         (color #:vec3)
                         (ambient #:vec3)
                         (n-lights #:int)
                         (light-positions (#:array #:vec3 8))
                         (light-colors (#:array #:vec3 8))
                         (light-intensities (#:array #:float 8))
                         (material #:vec4))
               output: ((frag-color #:vec4)))
   (define (main) #:void
     (set! frag-color (light (vec4 color 1) p
                             (normalize (* (mat3 inverse-transpose-model) n)))))))

;; Stones
(define (add-stone-to-scene stone position)
  (let ((n (add-node (scene) phong-pipeline-render-pipeline
                     mesh: stone-mesh
                     color: (alist-ref (stone-color stone) colors)
                     material: shiny-material
                     position: (v+ position
                                   (make-point 0 0 (* stone-radius
                                                      0.4)))
                     radius: stone-radius)))
    (stone-scene-node-set! stone n)))

;; Board
(define (generate-board)
  (add-node (scene)
            board-shader-render-pipeline
            mesh: board-mesh
            material: shiny-material
            position: (make-point 0.5 0.5 -0.03)))

(define (generate-lines)
  (let* ((rect (rectangle-mesh 1 (/ 256)))
         (line (lambda (start end)
                 (let ((length (vector-magnitude (v- start end)))
                       (angle (atan (- (point-y end) (point-y start))
                                    (- (point-x end) (point-x start)))))
                   (cons rect
                         (translate start
                                    (rotate-z angle
                                              (translate
                                               (make-point (* length 0.5) 0 0)
                                               (3d-scaling length 1 1))))))))
         (make-lines (lambda (nodes)
                       (append-map (lambda (row)
                                     (let loop ((points (map node-point row)))
                                       (if (null? (cdr points))
                                           '()
                                           (cons (line (car points) (cadr points))
                                                 (loop (cdr points))))))
                                   nodes)))
         (node-rows (chop (state-nodes (game-state))
                          (n-nodes)))
         (e-w-lines (make-lines node-rows))
         (n-s-lines (make-lines (apply zip node-rows)))
         (3nodes (+ (/ 3 (n-nodes))
                    (/ 120)))
         (16nodes (- (/ 16 (n-nodes))
                     (/ 120)))
         (marker-points `((,3nodes . ,3nodes) (,3nodes . 0.5) (,3nodes . ,16nodes)
                          (0.5 . ,3nodes) (0.5 . 0.5) (0.5 . ,16nodes)
                          (,16nodes . ,3nodes) (,16nodes . 0.5) (,16nodes . ,16nodes)))
         (markers (map (lambda (p)
                         (cons marker
                               (translation (make-point (car p) (cdr p) 0))))
                       marker-points)))
    (lines (mesh-transform-append 'position (append e-w-lines n-s-lines
                                                    markers)))
    (add-node (scene) mesh-pipeline-render-pipeline
              mesh: (lines)
              color: (alist-ref 'black colors)
              position: (make-point 0 0 0.001))))

;; Score
(define score-font (make-parameter #f))

(define (update-score)
  (let* ((s (get-score))
         (black (string-append "Black: " (number->string (alist-ref 'black s))))
         (white (string-append "White: " (number->string (alist-ref 'white s)))))
    (when (score)
      (delete-node (score)))
    (score-mesh (string-mesh (string-append black "  " white) (score-font)))
    (score (add-node ui text-pipeline-render-pipeline
                     tex: (face-atlas (score-font))
                     color: (alist-ref 'black colors)
                     position: (make-point 10 -10 0)
                     mesh: (score-mesh)))))

;;;
;;; Input
;;;
(define keys (make-bindings
              `((quit ,+key-escape+ press: ,stop))))

(define (get-cursor-board-position)
  (receive (near far) (get-cursor-world-position (camera))
    (let ((u (/ (point-z near) (- (point-z near) (point-z far)))))
      (make-point (+ (point-x near) (* u (- (point-x far) (point-x near))))
                  (+ (point-y near) (* u (- (point-y far) (point-y near))))
                  0))))

(define (get-nearest-index)
  (let ((n (vround (v* (vclamp (get-cursor-board-position) 0 1)
                       (sub1 (n-nodes))))))
    (cons (inexact->exact (point-x n))
                    (inexact->exact (point-y n)))))

(define (cursor-board-press)
  (place-stone (get-nearest-index))
  (update-score))

(define mouse (make-bindings
               `((left-click ,+mouse-button-left+ press: ,cursor-board-press))))

;;;
;;; Initialization
;;;
(define (init)
  (push-key-bindings keys)
  (push-mouse-bindings mouse)
  (gl:clear-color 0.8 0.8 0.8 1)
  (scene (make-scene))
  (activate-extension (scene) (lighting))
  (set-ambient-light! (scene) (make-point 0.4 0.4 0.4))
  (let ((light (add-light (scene) (make-point 1 1 1) 100)))
    (set-node-position! light (make-point 0 0 2)))
  (camera (make-camera #:perspective #:position (scene) near: 0.001 angle: 35))

  (set-camera-position! (camera) (make-point 1.6 -0.6 1.3))
  (quaternion-rotate-z (degrees->radians 45)
                       (quaternion-rotate-x (degrees->radians 45)
                                            (camera-rotation (camera))))
  (generate-board)
  (generate-state)
  (generate-lines)

  (score-font (load-face font 20))
  (update-score))

(start 800 600 "Go" resizable: #f init: init)
