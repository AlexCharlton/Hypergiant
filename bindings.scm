(export make-bindings
        add-binding!
        get-binding
        remove-binding!
        set-binding!
        key-bindings
        push-key-bindings
        pop-key-bindings
        mouse-bindings
        push-mouse-bindings
        pop-mouse-bindings
        cursor-movement-callback)

(define-record binding
  id key scancode? mods press release pressed?)

(define (bind id key #!key scancode? mods (press (lambda () #f)) (release (lambda () #f))
                toggle reverse-toggle)
    (let ((mods (if mods
                    (apply bitwise-ior mods)
                    0)))
      (cond
       (toggle
        (make-binding id key scancode? mods
                      (lambda () (toggle (add1 (toggle))))
                      (lambda () (toggle (sub1 (toggle))))
                      #f))
       (reverse-toggle
        (make-binding id key scancode? mods
                      (lambda () (reverse-toggle (sub1 (reverse-toggle))))
                      (lambda () (reverse-toggle (add1 (reverse-toggle))))
                      #f))
       (else (make-binding id key scancode? mods press release #f)))))

(define (make-bindings bindings)
  (map (lambda (b) (apply bind b)) bindings))

(define (add-binding! bindings binding)
  (cons (apply bind binding)
        bindings))

(define (get-binding bindings id)
  (find (lambda (b) (equal? (binding-id b) id))
        bindings))

(define (remove-binding! bindings id)
  (remove (lambda (b) (equal? (binding-id b) id))
          bindings))

(define (set-binding! bindings id binding)
  (if* (find (lambda (b) (equal? (binding-id b) id))
             bindings)
       (cons (apply bind bindings)
             (delete it bindings))
       (error 'set-binding! "Not a binding id:" id)))


;;; Key bindings
(define key-bindings (make-parameter '()))

(define (push-key-bindings bindings)
  (key-bindings (cons bindings
                      (key-bindings))))

(define (pop-key-bindings)
  (unless (null? (key-bindings))
    (key-bindings (cdr (key-bindings)))))

(define (key-event window key scancode action mods)
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

;;; Mouse bindings
(define mouse-bindings (make-parameter '()))

(define (push-mouse-bindings bindings)
  (mouse-bindings (cons bindings
                       (mouse-bindings))))

(define (pop-mouse-bindings)
  (unless (null? (mouse-bindings))
    (mouse-bindings (cdr (mouse-bindings)))))


(define (mouse-click window button action mods)
  (when (not (null? (key-bindings)))
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

(define (mouse-move window x y)
  ((cursor-movement-callback) x y))
