(export make-bindings
        push-bindings
        pop-bindings
        )

(define keybindings (make-parameter '()))

(define-record binding
  key scancode? mods press release pressed?)

(define (push-bindings bindings)
  (keybindings (cons bindings keybindings)))

(define (pop-bindings)
  (unless (null? (keybindings))
    (keybindings (cdr (keybindings)))))

(define (make-bindings bindings)
  (define (bind key #!key scancode? mods (press (lambda () #f)) (release (lambda () #f))
                toggle reverse-toggle)
    (let ((mods (if mods
                    (apply bitwise-ior mods)
                    0)))
      (cond
       (toggle
        (make-binding key scancode? mods
                      (lambda () (toggle (add1 (toggle))))
                      (lambda () (toggle (sub1 (toggle))))
                      #f))
       (reverse-toggle
        (make-binding key scancode? mods
                      (lambda () (reverse-toggle (sub1 (reverse-toggle))))
                      (lambda () (reverse-toggle (add1 (reverse-toggle))))
                      #f))
       (else (make-binding key scancode? mods press release #f)))))
  (let ((vec (make-vector (length bindings))))
    (for-each (lambda (b i)
                (vector-set! vec i
                             (apply bind b)))
              bindings (iota (length bindings)))
    vec))

; TODO:
(define (add-to-bindings! bindings binding)
  #f)
#;(define (remove-from-bindings! bindings))
#;(define (set-binding! bindings))

(define (key-event window key scancode action mods)
  (when (not (null? (keybindings)))
    (let* ((bindings (car (keybindings)))
           (len (vector-length bindings)))
      (cond
       ((= action +press+)
        (let loop ((i 0))
          (let ((binding (vector-ref bindings i)))
            (if (and (if (binding-scancode? binding)
                       (= (binding-key binding) scancode)
                       (= (binding-key binding) key)) 
                   (= (binding-mods binding) mods))
                (begin ((binding-press binding))
                       (binding-pressed?-set! binding #t))
                (when (< (add1 i) len)
                  (loop (add1 i)))))))
       ((= action +release+)
        (let loop ((i 0))
          (let ((binding (vector-ref bindings i)))
            (when (and (if (binding-scancode? binding)
                         (= (binding-key binding) scancode)
                         (= (binding-key binding) key))
                     (binding-pressed? binding))
              ((binding-release binding))
              (binding-pressed?-set! binding #f))
            (when (< (add1 i) len)
              (loop (add1 i))))))))))
