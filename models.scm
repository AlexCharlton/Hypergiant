;;;; models.scm

;;;; IQM model loading and support for skeletal animations

(module hypergiant-models
()

(import chicken scheme foreign)
(use (prefix gl-utils gl:) (except bitstring bitstring->vector)
     data-structures srfi-1 srfi-4 srfi-99 miscmacros)

(include "iqm")

;(define cube (load-iqm "tests/cube.iqm"))
;(print cube)
;(print (iqm->mesh cube '(position)))
;(load-iqm "tests/Frankie.iqm")


) ; end module hypergiant-models
