(use test hypergiant miscmacros lolevel srfi-1)

(test-begin "load-iqm")
(test-assert (load-iqm "./cube.iqm"))
(test-error (load-iqm "../hypergiant.scm"))
(test-end)


;; Utils
(define delta 0.0005)
(define mrfixit (load-iqm "./mrfixit.iqm"))
(define n-joints (length (iqm-joints mrfixit)))

(define (nth-3x4-matrix m n) (pointer+ m (* 4 12 n)))

(define (nth-matrix m n) (pointer+ m (* 4 16 n)))

(define (make-matrix-array n) (allocate (* 4 16 n)))

(define (matrix-ref m k)
  (pointer-f32-ref (pointer+ m (* 4 k))))

(define (print-mat3x4 m)
  (print "[" (matrix-ref m 0) " " (matrix-ref m 1) " "
         (matrix-ref m 2) " " (matrix-ref m 3) " ")
  (print (matrix-ref m 4) " " (matrix-ref m 5) " "
         (matrix-ref m 6) " " (matrix-ref m 7) " ")
  (print (matrix-ref m 8) " " (matrix-ref m 9) " "
         (matrix-ref m 10) " " (matrix-ref m 11) "]"))

(define (file->u8vector file)
  (with-input-from-file file
    (lambda ()
      (read-u8vector #f))))

(define m4 (allocate 16))
(define (m4x4-3x4-eq? m4x4 m3x4)
  (transpose m4x4 m4)
  (fold (lambda (i r)
          (and r (< (abs (- (matrix-ref m3x4 i)
                            (matrix-ref m4 i)))
                    delta))) 
        #t
        (iota 12)))


(test-begin "iqm base frame")
(define base-frames* (file->u8vector "./base-frames"))
(define cannonical-base-frames(gl:->pointer base-frames*))
(define inverse-base-frames* (file->u8vector "./inverse-base-frames"))
(define cannonical-inverse-base-frames(gl:->pointer inverse-base-frames*))
(define hpg-base-frames (iqm-base-frame mrfixit))
(define hpg-inverse-base-frames (iqm-inverse-base-frame mrfixit))

(define (compare-frames fc f)
  (dotimes (i n-joints)
    (unless (m4x4-3x4-eq? (nth-matrix f i)
                          (nth-3x4-matrix fc i))
      (print "Frame " i " incorrect: ")
      (print-mat4 (nth-matrix f i))
      (print "Should equal")
      (print-mat3x4 (nth-3x4-matrix fc i))
      (error 'matrices-not-equal)))
  #t)

(test-assert "base frames are correct"
             (compare-frames cannonical-base-frames
                             hpg-base-frames))
(test-assert "inverse base frames are correct"
             (compare-frames cannonical-inverse-base-frames
                             hpg-inverse-base-frames))
(test-end)


(test-begin "iqm animation matrices")
(define pose-matrices* (file->u8vector "./pose-matrices"))
(define cannonical-pose-matrices (gl:->pointer pose-matrices*))
(define anim (alist-ref 'idle (iqm-animations mrfixit)))
(define n-frames (animation-n-frames anim))
(define hpg-pose-matrices (car (animation-frames anim)))

(define (compare-matrices)
  (dotimes (i n-frames)
    (dotimes (j n-joints)
      (let ((k (+ j (* i n-joints))))
        (unless (m4x4-3x4-eq? (nth-matrix hpg-pose-matrices k)
                              (nth-3x4-matrix cannonical-pose-matrices k))
          (print "Joint " j " of frame " i " incorrect: ")
          (print-mat4 (nth-matrix hpg-pose-matrices k))
          (print "Should equal")
          (print-mat3x4 (nth-3x4-matrix cannonical-pose-matrices k))
          (error 'matrices-not-equal)))))
  #t)

(test-assert (compare-matrices))
(test-end)


(test-begin "iqm animated matrices")
(define animated-matrices* (file->u8vector "./animated-matrices"))
(define cannonical-animated-matrices (gl:->pointer animated-matrices*))
(define anim (alist-ref 'idle (iqm-animations mrfixit)))
(define hpg-animated-matrices (make-matrix-array n-joints))
(animate-model-frames anim hpg-animated-matrices n-joints 0 0 0)

(define (compare-matrices)
  (dotimes (i n-joints)
    (unless (m4x4-3x4-eq? (nth-matrix hpg-animated-matrices i)
                          (nth-3x4-matrix cannonical-animated-matrices i))
      (print "Joint " i " incorrect: ")
      (print-mat4 (nth-matrix hpg-animated-matrices i))
      (print "Should equal")
      (print-mat3x4 (nth-3x4-matrix cannonical-animated-matrices i))
      (error 'matrices-not-equal)))
  #t)

(test-assert (compare-matrices))
(test-end)

(test-exit)
