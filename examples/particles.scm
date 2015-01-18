;;;; paricles.scm

;;;; This example illustrates Hypergiant's particle system.

;;;; NOTE:
;;;; If this file is compiled, since it uses glls-render, it must also be linked with OpenGL
;;;; E.g.:
;;;; csc -lGL particles.scm

;;;; Use arrow keys (and shift) to rotate, zoom camera.

(import chicken scheme)
(use hypergiant)

(define scene (make-parameter #f))
(define camera (make-parameter #f))
(define emitter (make-parameter #f))
(define pan (make-parameter 0))
(define tilt (make-parameter 0))
(define zoom (make-parameter 0))
(define c-roll (make-parameter 0))

(define keys (make-bindings
              `((quit ,+key-escape+ press: ,stop)
                (tilt-up ,+key-up+ toggle: ,tilt)
                (tilt-down ,+key-down+ reverse-toggle: ,tilt)
                (zoom-in ,+key-up+ mods: (,+mod-shift+) reverse-toggle: ,zoom)
                (zoom-out ,+key-down+ mods: (,+mod-shift+) toggle: ,zoom)
                (pan-right ,+key-right+ toggle: ,pan)
                (pan-left ,+key-left+ reverse-toggle: ,pan)
                (roll-right ,+key-right+ mods: (,+mod-shift+) reverse-toggle: ,c-roll)
                (roll-left ,+key-left+ mods: (,+mod-shift+) toggle: ,c-roll))))

(define-alpha-pipeline particle-pipeline
  ((#:vertex input: ((position #:vec3) (age #:float))
             uniform: ((mvp #:mat4))
             output: ((c #:vec4)))
   (define (main) #:void
     (set! gl:position (* mvp (vec4 position 1.0)))
     (set! gl:point-size 2)
     (set! c (vec4 0 0 0 (clamp (/ 1.0 age) 0 0.4)))))
  ((#:fragment input: ((c #:vec4))
	       output: ((frag-color #:vec4)))
   (define (main) #:void
     (set! frag-color c))))

(define max-age 20)
(define create-timer (make-parameter 0))
(define create-delay 0)

(define (init)
  (gl:enable gl:+cull-face+)
  (gl:enable gl:+program-point-size+)
  (push-key-bindings keys)
  (gl:clear-color 0.9 0.9 1.0 1)
  (scene (make-scene))
  (activate-extension (scene) (particles))
  (camera (make-camera #:perspective #:orbit (scene)))
  (camera-look-at! (camera) (make-point 0 0 0))
  (set-camera-zoom! (camera) 4)
  (emitter (add-emitter (scene) particle-pipeline-render-pipeline
                        attributes: '((age #:float 1))
                        n-particles: 20000))
  (set! create-delay (/ max-age (emitter-max-particles (emitter)))))

(define (update delta)
  (let ((global-vector (make-point (* 0.001 (sin (* 0.5 (get-time))))
                                   (* delta 0.1)
                                   0)))
    (for-emitter (p (emitter) (position age))
      (if (and (< (emitter-n-particles (emitter))
                  (emitter-max-particles (emitter)))
               (> (create-timer) create-delay))
          (let ((new (add-particle (emitter))))
            (create-timer 0)
            (position-set! new (make-point 0 0 0))
            (age-set! new (f32vector 0)))
          (create-timer (+ (create-timer) delta)))
      (let ((age (f32vector-ref (age-ref p) 0)))
        (if (> age max-age)
            (delete-particle (emitter) p)
            (begin
              (age-set! p (f32vector (+ age delta)))
              (position-set! p (v+ (v+ (position-ref p)
                                       (v* (make-point (random-float)
                                                       0
                                                       (random-float))
                                           (* delta 0.2)))
                                   global-vector)))))))

  (yaw-camera! (camera) (/ (pan) 30))
  (pitch-camera! (camera) (/ (tilt) 30))
  (roll-camera! (camera) (/ (c-roll) 30))
  (zoom-camera! (camera) (/ (zoom) 10)))

(start 640 480 "Particles" resizable: #f init: init update: update)
