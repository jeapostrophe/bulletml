#lang scheme
(require 2htdp/universe
         "posn.ss"
         "dl.ss"
         "engine.ss")

(define tick-rate 4/60)

(define (place-at-posn i p s)
  (define cp (posn->cartesian-posn p))
  (place-image i
               (cartesian-posn-x cp)
               (cartesian-posn-y cp)
               s))

(define (simulate bml)
  (define height 600)
  (define width 800)
  (define bls (load-bml width height bml))
  
  (define (stop-bullets? frame) 
    (define bs (bullets-bs bls))
    (empty? bs))
  (define (draw-bullets frame)
    (define bs (bullets-bs bls))
    (define target-posn (bullets-target-posn bls))
    (for/fold ([s (place-at-posn (circle 3 'outline "red")
                                 target-posn
                                 (place-image (text (format "Bullets: ~a" (length bs))
                                                    12 "black")
                                              0 0
                                              (empty-scene width height)))])
      ([b (in-list bs)])
      (place-at-posn (circle 2 'solid "black")
                     (body-posn b)
                     s)))
  (define (tick-bullets frame)
    (define next-frame (add1 frame))
    (step! bls)
    next-frame)
  (define (move-target frame x y me)
    (set-bullets-target-posn! bls (make-cartesian-posn x y))
    frame)
  
  (big-bang
   0
   (on-tick tick-bullets
            tick-rate)
   (on-draw draw-bullets)
   (on-mouse move-target)
   (stop-when stop-bullets?)))

(provide/contract
 [current-rank (parameter/c (real-in 0 1))]
 [simulate (bulletdl? . -> . void)])