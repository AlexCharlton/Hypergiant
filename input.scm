(export make-bindings
        make-binding
        binding?
        binding-id
        add-binding
        get-binding
        remove-binding
        change-binding
        key-bindings
        push-key-bindings
        pop-key-bindings
        char-callback
        mouse-bindings
        push-mouse-bindings
        pop-mouse-bindings
        cursor-movement-callback
        scroll-callback
        get-cursor-position
        set-cursor-position
        get-cursor-world-position)

(define-record-type binding
  %make-binding
  #t
  (id) (key) (scancode?) (mods) (press) (release) (pressed?))

(define-record-printer (binding b out)
  (fprintf out "#<binding ~S>"
    (binding-id b)))

(define (make-binding id key #!key scancode? mods (press (lambda () #f)) (release (lambda () #f))
                      toggle reverse-toggle)
    (let ((mods (if mods
                    (apply bitwise-ior mods)
                    0)))
      (cond
       (toggle
        (%make-binding id key scancode? mods
                      (lambda () (toggle (add1 (toggle))))
                      (lambda () (toggle (sub1 (toggle))))
                      #f))
       (reverse-toggle
        (%make-binding id key scancode? mods
                      (lambda () (reverse-toggle (sub1 (reverse-toggle))))
                      (lambda () (reverse-toggle (add1 (reverse-toggle))))
                      #f))
       (else (%make-binding id key scancode? mods press release #f)))))

(define (make-bindings bindings)
  (map (lambda (b) (apply make-binding b)) bindings))

(define (add-binding bindings binding)
  (cons (apply make-binding binding)
        bindings))

(define (get-binding bindings id)
  (find (lambda (b) (equal? (binding-id b) id))
        bindings))

(define (remove-binding bindings id)
  (remove (lambda (b) (equal? (binding-id b) id))
          bindings))

(define (change-binding bindings id binding)
  (if* (find (lambda (b) (equal? (binding-id b) id))
             bindings)
       (cons (apply make-binding binding)
             (delete it bindings))
       (error 'set-binding! "Not a binding id:" id)))

;;; Keyboard
(define key-bindings (make-parameter '()))

(define (push-key-bindings bindings)
  (cond
   ((list? bindings)
    (key-callback key-binding-event))
   (else (key-callback bindings)))
  (key-bindings (cons bindings
                      (key-bindings))))

(define (pop-key-bindings)
  (unless (null? (key-bindings))
    (key-bindings (cdr (key-bindings)))
    (unless (null? (key-bindings))
      (cond
       ((list? (car (key-bindings)))
        (key-callback key-binding-event))
       (else (key-callback (car (key-bindings))))))))

(define (key-binding-event key scancode action mods)
  (when (not (null? (key-bindings)))
    (let ((bindings (car (key-bindings))))
      (cond
       ((= action %+press+)
        (let loop ((bindings bindings))
          (unless (null? bindings)
            (let ((binding (car bindings)))
              (if (and (if (binding-scancode? binding)
                         (= (binding-key binding) scancode)
                         (= (binding-key binding) key)) 
                     (= (binding-mods binding) mods))
                  (begin ((binding-press binding))
                         (binding-pressed?-set! binding #t))
                  (loop (cdr bindings)))))))
       ;; Release for all bindings that share the same key, even with different mods
       ((= action %+release+)
        (for-each (lambda (binding)
                    (when (and (if (binding-scancode? binding)
                                 (= (binding-key binding) scancode)
                                 (= (binding-key binding) key))
                             (binding-pressed? binding))
                      ((binding-release binding))
                      (binding-pressed?-set! binding #f)))
                  bindings))))))

(define key-callback (make-parameter (lambda (key scancode action mods) #f)))
(define char-callback (make-parameter (lambda (char) #f)))

(define-external (hpgKeyCallback (c-pointer window) (int key) (int scancode)
                                 (int action) (int mods))
    void
  ((key-callback) key scancode action mods))

(define-external (hpgCharCallback (c-pointer window) (unsigned-int char))
  void
  ((char-callback) char))


;;; Mouse
(define mouse-bindings (make-parameter '()))

(define (push-mouse-bindings bindings)
  (mouse-bindings (cons bindings
                       (mouse-bindings))))

(define (pop-mouse-bindings)
  (unless (null? (mouse-bindings))
    (mouse-bindings (cdr (mouse-bindings)))))

(define (mouse-click window button action mods)
  (when (not (null? (mouse-bindings)))
    (let ((bindings (car (mouse-bindings))))
      (cond
       ((= action %+press+)
        (let loop ((bindings bindings))
          (unless (null? bindings)
            (let ((binding (car bindings)))
              (if (and (= (binding-key binding) button) 
                     (= (binding-mods binding) mods))
                  (begin ((binding-press binding))
                         (binding-pressed?-set! binding #t))
                  (loop (cdr bindings)))))))
       ;; Release for all bindings that share the same button, even with different mods
       ((= action %+release+)
        (for-each (lambda (binding)
                    (when (and (= (binding-key binding) button)
                             (binding-pressed? binding))
                      ((binding-release binding))
                      (binding-pressed?-set! binding #f)))
                  bindings))))))

(define cursor-movement-callback (make-parameter (lambda (x y) #f)))
(define scroll-callback (make-parameter (lambda (x y) #f)))

(define-external (hpgCursorPositionCallback (c-pointer window)
                                            (double x) (double y))
    void
  ((cursor-movement-callback) x y))


(define-external (hpgScrollCallback (c-pointer window)
                                    (double x) (double y))
    void
  ((scroll-callback) x y))

(define (get-cursor-position)
  (%get-cursor-position (%window)))

(define (set-cursor-position x y)
  (%set-cursor-position (%window) x y))

(define (get-cursor-world-position camera)
  (define (scale x) (sub1 (* x 2)))
  (let-values (((w h) (get-window-size))
               ((x y) (get-cursor-position)))
    (let* ((ivp (make-f32vector 16))
           (x (scale (/ x w)))
           (y (scale (- 1 (/ y h))))
           (near (make-point x y -1))
           (far (make-point x y 1)))
      (inverse (scene:camera-view-projection camera) (->pointer ivp))
      (m*vector! ivp near)
      (m*vector! ivp far)
      (values near far))))
