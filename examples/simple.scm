;;;; simple.scm

;;;; This example renders a square mesh

(import chicken scheme)
(use hypergiant)

(define scene (make-parameter #f))
(define camera (make-parameter #f))

(define square (rectangle-mesh 1 1 color: (lambda (i)
                                            (list-ref '((1 0 0)
                                                        (0 1 0)
                                                        (0 0 1)
                                                        (1 0 1))
                                                      i))))

(define (init)
  (scene (make-scene))
  (camera (make-camera #:perspective #:orbit (scene)))
  (add-node (scene) color-pipeline-render-pipeline
            mesh: square))

(start 400 400 "Simple" init: init resizable: #f)
