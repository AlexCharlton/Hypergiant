(import chicken scheme)

(use hypergiant miscmacros srfi-42 srfi-99 data-structures srfi-1 srfi-69)

;;; Rules
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
  (let* ((jitter (/ 1 (n-nodes) 3))
         (nodes (list-ec (: i (n-nodes))
                         (: j (n-nodes))
                         (make-node (cons j i)
                                    (make-point (+ (/ j (sub1 (n-nodes)))
                                                   (* jitter (random-float)))
                                                (+ (/ i (sub1 (n-nodes)))
                                                   (* jitter (random-float)))
                                                0.0003)
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

;; Stone
(define stone-types '(0 1 2 3 4 5))

(define (next-type type)
  (let ((next-types (member type stone-types)))
    (if (= (length next-types) 1)
        (car next-types)
        (cadr next-types))))

(define-record-type stone
  #t #t
  color
  type
  (mesh)
  (scene-node))

(define (delete-stone node)
  (node-stone-set! node #f)
  (update-chains-with-node node))

(define (add-stone node color type)
  (when (node-stone node)
    (signal (make-property-condition 'game-logic 'occupied node)))
  (node-stone-set! node (make-stone color type #f #f))
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
                 (add-stone node color (car stone-types))
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
    score))

(define (display-score)
  (print (get-score)))

;;; Rendering
(define board-rect (rectangle-mesh 1 1 centered: #f
                                   texture-width: 1 texture-height: 1))

(define scene (make-parameter #f))
(define camera (make-parameter #f))

(define-pipeline noise-shader
  ((#:vertex input: ((position #:vec3))
             output: ((pos #:vec2))) 
   (define (main) #:void
     (set! pos (* (~~ position x y) 2))
     (set! gl:position (vec4 (- pos
                                1)
                             0 1))))
  ((#:fragment input: ((pos #:vec2))
               output: ((frag-color #:vec4))
               use: (simplex-noise-2d))
   (define (grass) #:vec4
     (let ((n #:float (* (snoise (* pos 128)) 0.2)))
       (+ (vec4 0.2 0.5 0 1) n)))
   (define (dirt) #:vec4
     (let ((n #:float (* (snoise (* pos 4)) 0.1)))
       (+ (vec4 1 0.8 0.4 1) n)))
   (define (main) #:void
     (let ((n #:float (clamp (* 1.3 (+ 0.4 (abs (fractal-snoise pos 5 1 0.5 2 0.5))))
                             0 1)))
       (set! frag-color (+ (* (grass) n)
                           (* (dirt) (- 1 n))))))))

(define-pipeline board-shader
  ((#:vertex input: ((position #:vec3) (tex-coord #:vec2))
             uniform: ((mvp #:mat4))
             output: ((tex-c #:vec2)))
   (define (main) #:void
     (let ((pos #:vec2 (~~ position x y)))
       (set! gl:position (* mvp (vec4 (- (* pos 1.2) 0.1) 0 1.0)))
       (set! tex-c tex-coord))))
  ((#:fragment input: ((tex-c #:vec2))
               uniform: ((tex #:sampler-2d))
               output: ((frag-color #:vec4)))
   (define (main) #:void
     (set! frag-color (texture tex tex-c)))))

(define colors '((white (1 1 1) (1 0.8 0.8) (1 0.6 0.6)
                        (1 0.4 0.4) (1 0.2 0.2) (1 0 0))
                 (black (0 0 0) (0 0 0.2) (0 0 0.4)
                        (0 0 0.6) (0 0 0.8) (0 0 1))))

(define (stone-mesh color type)
  (let ((color (list-ref (alist-ref color colors) type)))
    (rectangle-mesh (/ 30) (/ 30)
                    color-type: #:uchar
                    color: (lambda (_) color))))

(define (add-stone-to-scene stone position)
  (let* ((mesh (stone-mesh (stone-color stone)
                              (stone-type stone)))
         (n (add-node (scene) color-pipeline-render-pipeline
                      mesh: mesh)))
    (set-node-position! n (v+ position (make-point 0 0 0.0003)))
    (stone-mesh-set! stone mesh)
    (stone-scene-node-set! stone n)))

(define brown (f32vector 0.8 0.6 0.4))
(define lines (make-parameter #f))

(define (generate-lines)
  (let* ((rect (rectangle-mesh 1 (/ 1 256)))
         (road (lambda (start end)
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
                                           (cons (road (car points) (cadr points))
                                                 (loop (cdr points))))))
                                   nodes)))
         (node-rows (chop (state-nodes (game-state))
                          (n-nodes)))
         (e-w-lines (make-lines node-rows))
         (n-s-lines (make-lines (apply zip node-rows))))
    (lines (mesh-transform-append 'position (append e-w-lines n-s-lines)))
    (add-node (scene) mesh-pipeline-render-pipeline
              mesh: (lines)
              color: brown)))

(define (generate-map)
  (define map-rect (rectangle-mesh 1 1 centered: #f))
  (mesh-make-vao! map-rect (pipeline-mesh-attributes noise-shader))
  (let ((renderable (make-noise-shader-renderable mesh: map-rect)))
    (receive (fbo tex r) (gl:create-framebuffer 512 512)
      (gl:with-framebuffer fbo
        (gl:clear (bitwise-ior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))
        (render-noise-shader renderable))
      (gl:delete-renderbuffer r)
      (gl:delete-framebuffer fbo)
      tex)))

(define (generate-board)
  (let ((tex (generate-map)))
    (add-node (scene)
              board-shader-render-pipeline
              mesh: board-rect
              tex: tex)))

;;; Input
(define pan-x (make-parameter 0))
(define pan-y (make-parameter 0))
(define zoom (make-parameter 0))
(define pan-speed (/ 1 100))

(define keys (make-bindings
              `((quit ,+key-escape+ press: ,stop)
                (pan-up ,+key-up+ toggle: ,pan-y)
                (pan-down ,+key-down+ reverse-toggle: ,pan-y)
                (zoom-in ,+key-up+ mods: (,+mod-shift+) reverse-toggle: ,zoom)
                (zoom-out ,+key-down+ mods: (,+mod-shift+) toggle: ,zoom)
                (pan-right ,+key-right+ toggle: ,pan-x)
                (pan-left ,+key-left+ reverse-toggle: ,pan-x)
                (score ,+key-s+ press: ,display-score))))

(define (get-cursor-board-position)
  (receive (near far) (get-cursor-world-position)
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
  (place-stone (get-nearest-index)))

(define mouse (make-bindings
               `((left-click ,+mouse-button-left+ press: ,cursor-board-press))))

(define (update delta)
  (move-camera! (camera)
               (make-point (+ (* (pan-x) pan-speed)
                              (* (- (pan-y)) pan-speed)
                              (* (zoom) pan-speed))
                           (+ (* (pan-x) pan-speed)
                              (* (pan-y) pan-speed)
                              (* (- (zoom)) pan-speed))
                           (* (zoom) pan-speed))))

;;; Initialization
(define (init)
  (push-key-bindings keys)
  (push-mouse-bindings mouse)
  (gl:clear-color 0.02 0.01 0.04 1)
  (scene (make-scene))
  (camera (make-camera #:perspective #:position (scene) near: 0.001 angle: 45))

  (set-camera-position! (camera) (make-point 1.4 -0.4 1))
  (quaternion-rotate-z (degrees->radians 45)
                       (quaternion-rotate-x (degrees->radians 45)
                                            (camera-rotation (camera))))
  (generate-board)
  (generate-state)
  (generate-lines))

(start 800 600 "Go" resizable: #f init: init update: update)
