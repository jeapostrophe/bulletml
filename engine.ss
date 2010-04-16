#lang scheme
(require "posn.ss"
         "dl.ss")

;; My implementation is subtly different because I conflate action objs and bullet objs

(define-struct body (posn velocity 
                          speed-change speed-change-frames
                          dir-change dir-change-frames
                          accel accel-frames
                          previous-fire params
                          action) #:mutable)

(define current-rank (make-parameter 1))
(define current-params (make-parameter (vector)))

(define Expression->value
  (match-lambda
    [(struct Expression (value-f))
     (if (number? value-f)
         value-f
         (value-f (current-rank) (current-params)))]))
(define (value->Expression v)
  (make-Expression v))

(define (posn-speed p)
  (polar-posn-r (posn->polar-posn p)))
(define (body-speed b)
  (posn-speed (body-velocity b)))
(define (body-previous-fire-speed b)
  (posn-speed (body-previous-fire b)))

(define (posn-direction p)
  (polar-posn-theta (posn->polar-posn p)))
(define (body-direction b)
  (posn-direction (body-velocity b)))
(define (body-previous-fire-direction b)
  (posn-direction (body-previous-fire b)))

(define-syntax-rule (speed->value rel seq s)
  (match s
    [(struct speed (type expr))
     (define v (Expression->value expr))
     (case type
       [(absolute)
        v]
       [(relative)
        (+ v rel)]
       [(sequence)
        (+ v seq)])]))
(define-syntax-rule (direction->value aim rel seq d)
  (match d
    [(struct direction (type expr))
     (define v (degrees->radians (Expression->value expr)))
     (case type
       [(aim)
        (+ v aim)]
       [(absolute)
        v]
       [(relative)
        (+ v rel)]
       [(sequence)
        (+ v seq)])]))

(define (aim-at p1 p2)
  (define cp1 (posn->cartesian-posn p1))
  (define cp2 (posn->cartesian-posn p2))
  (define dx
    (- (cartesian-posn-x cp1)
       (cartesian-posn-x cp2)))
  (define dy
    (- (cartesian-posn-y cp1)
       (cartesian-posn-y cp2)))
  (atan dy dx))

(define-struct bullets
  (width height [target-posn #:mutable] [bs #:mutable]))

(define (load-bml width height bdl)
  (define target-posn 
    (make-cartesian-posn 
     (/ width 2)
     (/ height 2)))
  
  (define top-posn 
    (case (bulletdl-type bdl)
      [(vertical)
       (make-cartesian-posn (/ width 2) 10)]
      [(horizontal)
       (make-cartesian-posn (- width 10) (/ height 2))]))
  
  (let ([bs (list (make-body top-posn polar-posn0
                             0 0 
                             0 0
                             cartesian-posn0 0
                             polar-posn0 (vector)
                             (bulletdl-top bdl)))])
    
    (make-bullets
     width height
     target-posn bs)))

(define (on-screen? width height p)
  (define cp (posn->cartesian-posn p))
  (and (< 0 (cartesian-posn-x cp) width)
       (< 0 (cartesian-posn-y cp) height)))

(define (act! bls a b)
  (match a
    [(struct repeat (times inner))
     (define new-times-v (round (sub1 (Expression->value times))))
     (act! bls
           (if (zero? new-times-v)
               inner
               (make-seqn (list inner
                                (make-repeat (value->Expression new-times-v) inner))))
           b)]
    [(struct fire (direction speed bullet))
     (define speed-src (or speed (bullet-speed bullet)))
     (define the-speed
       (if speed-src
           (local [(define prv-speed (body-previous-fire-speed b))]
             (speed->value prv-speed prv-speed speed-src))
           1))
     (define dir-src (or direction (bullet-direction bullet)))
     (define the-dir
       (if dir-src
           (direction->value (aim-at (bullets-target-posn bls) (body-posn b))
                             (body-direction b)
                             (body-previous-fire-direction b)
                             dir-src)
           (aim-at (bullets-target-posn bls) (body-posn b))))
     (define new-velocity
       (make-polar-posn the-speed the-dir))
     
     (set-body-previous-fire! b new-velocity)
     (set-bullets-bs!
      bls
      (cons
       (make-body (body-posn b) new-velocity 
                  0 0
                  0 0
                  cartesian-posn0 0
                  polar-posn0
                  (bullet-params bullet)
                  (bullet-action bullet))
       (bullets-bs bls)))
     #f]
    [(struct changeSpeed (speed frames))
     (define term-v (round (Expression->value frames)))
     (define speed-v (speed->value (body-speed b) 0 speed))
     (define change-in-speed
       (/ (- speed-v (body-speed b))
          term-v))
     
     (set-body-speed-change! b change-in-speed)
     (set-body-speed-change-frames! b term-v)
     #f]
    [(struct changeDirection (direction frames))
     (define term-v (round (Expression->value frames)))
     (define direction-v 
       (direction->value (aim-at (bullets-target-posn bls) (body-posn b))
                         (body-direction b)
                         0
                         direction))
     (define change-in-direction
       (if (eq? 'sequence (direction-type direction))
           direction-v
           (/ (- direction-v (body-direction b))
              term-v)))
     
     (set-body-dir-change! b change-in-direction)
     (set-body-dir-change-frames! b term-v)
     #f]
    [(struct accel (hor vert frames))
     (define term-v (round (Expression->value frames)))
     (define dx
       (if hor
           (local [(define dx-v (Expression->value (horizontal-units hor)))]
             (case (horizontal-type hor)
               [(absolute)
                (/ (- dx-v (cartesian-posn-x (body-accel b)))
                   term-v)]
               [(relative)
                (/ dx-v term-v)]
               [(sequence)
                dx-v]))
           0))
     (define dy
       (if vert
           (local [(define dy-v (Expression->value (vertical-units vert)))]
             (case (vertical-type vert)
               [(absolute)
                (/ (- dy-v (cartesian-posn-y (body-accel b)))
                   term-v)]
               [(relative)
                (/ dy-v term-v)]
               [(sequence)
                dy-v]))
           0))
     
     (set-body-accel! b (make-cartesian-posn dx dy))
     (set-body-accel-frames! b term-v)
     #f]
    [(struct wait (times))
     (define new-times-v (round (sub1 (Expression->value times))))
     (if (zero? new-times-v)
         #f
         (make-wait (value->Expression new-times-v)))]
    [(struct vanish ())
     ; XXX O(n)!
     (set-bullets-bs! bls (remq b (bullets-bs bls)))
     #f]
    [(struct seqn (steps))
     (define new-step (act! bls (first steps) b))
     (if new-step
         (make-seqn (list* new-step (rest steps)))
         (if (empty? (rest steps))
             #f
             (make-seqn (rest steps))))]
    [(struct stalled-params (eparams inner))
     (act! bls
           (make-saved-params
            (build-vector (vector-length eparams)
                          (lambda (i)
                            (Expression->value (vector-ref eparams i))))
            inner)
           b)]
    [(struct saved-params (params inner))
     (define new-inner
       (parameterize ([current-params params])
         (act! bls inner b)))
     (if new-inner
         (make-saved-params params new-inner)
         #f)]
    [#f
     #f]))

(define (body-move! bls b)
  (define new-action 
    (parameterize ([current-params (body-params b)])
      (act! bls (body-action b) b)))
  (unless (zero? (body-speed-change-frames b))
    (set-body-speed-change-frames! b (sub1 (body-speed-change-frames b)))
    (set-body-velocity! b (posn+2 (body-velocity b) (make-polar-posn (body-speed-change b) 0))))
  (unless (zero? (body-dir-change-frames b))
    (set-body-dir-change-frames! b (sub1 (body-dir-change-frames b)))
    (set-body-velocity! b (posn+2 (body-velocity b) (make-polar-posn 0 (body-dir-change b)))))
  (unless (zero? (body-accel-frames b))
    (set-body-accel-frames! b (sub1 (body-accel-frames b)))
    (set-body-velocity! b (posn+2 (body-velocity b) (body-accel b))))
  (set-body-posn! b (posn+2 (body-posn b) (body-velocity b)))
  (set-body-action! b new-action))

; step! : bullets -> void
(define (step! bls)
  (for ([b (in-list (bullets-bs bls))])
    (body-move! bls b))
  
  ; XXX O(n)!
  (set-bullets-bs! bls
                   (filter (lambda (b)
                             (on-screen? (bullets-width bls)
                                         (bullets-height bls)
                                         (body-posn b)))
                           (bullets-bs bls)))
  
  (void))

(provide/contract
 [current-rank (parameter/c (real-in 0 1))]
 [bullets-bs (bullets? . -> . (listof body?))]
 [bullets-target-posn (bullets? . -> . (or/c cartesian-posn? polar-posn?))]
 [set-bullets-target-posn! (bullets? (or/c cartesian-posn? polar-posn?) . -> . void)]
 [body-posn (body? . -> . (or/c cartesian-posn? polar-posn?))]
 [load-bml (number? number? bulletdl? . -> . bullets?)]
 ; XXX Add new bullet/dead bullet callback
 [step! (bullets? . -> . void)])