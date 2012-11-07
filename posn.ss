#lang scheme
(require scheme/list
         scheme/math
         "fake-typed.ss")

(define-struct: cartesian-posn
  ([x : Number]
   [y : Number]))
(define-struct: polar-posn
  ([r : Number]
   [theta : Number]))

(define-type-alias posn (U cartesian-posn polar-posn))

(: cartesian-posn0 cartesian-posn)
(define cartesian-posn0 (make-cartesian-posn 0 0))
(: polar-posn0 polar-posn)
(define polar-posn0 (make-polar-posn 0 0))

(: cartesian-posn+2 (cartesian-posn cartesian-posn -> cartesian-posn))
(define (cartesian-posn+2 p1 p2)
  (make-cartesian-posn (+ (cartesian-posn-x p1) (cartesian-posn-x p2))
                       (+ (cartesian-posn-y p1) (cartesian-posn-y p2))))

(: cartesian-posn-scale (cartesian-posn Number -> cartesian-posn))
(define (cartesian-posn-scale p c)
  (make-cartesian-posn (* c (cartesian-posn-x p))
                       (* c (cartesian-posn-y p))))

(: polar-posn+2 (polar-posn polar-posn -> polar-posn))
(define (polar-posn+2 p1 p2)
  (make-polar-posn (+ (polar-posn-r p1) (polar-posn-r p2))
                   (+ (polar-posn-theta p1) (polar-posn-theta p2))))

(: polar-posn-scale (polar-posn Number -> polar-posn))
(define (polar-posn-scale p c)
  (make-polar-posn (* c (polar-posn-r p))
                   (* c (polar-posn-theta p))))

(: polar->cartesian (polar-posn -> cartesian-posn))
(define (polar->cartesian p)
  (make-cartesian-posn (* (polar-posn-r p) (cos (polar-posn-theta p)))
                       (* (polar-posn-r p) (sin (polar-posn-theta p)))))

(: cartesian->polar (cartesian-posn -> polar-posn))
(define (cartesian->polar p)
  (define x (cartesian-posn-x p))
  (define y (cartesian-posn-y p))
  (define r (sqrt (+ (expt x 2) (expt y 2))))
  (define theta
    (cond
      [(and (= x 0) (= y 0))
       0]
      [(x . >= . 0)
       (asin (/ y r))]
      [else
       (- pi (asin (/ y r)))]))       
  (make-polar-posn r theta))

(: posn->cartesian-posn (posn -> cartesian-posn))
(define (posn->cartesian-posn p)
  (if (cartesian-posn? p) p
      (polar->cartesian p)))

(: posn->polar-posn (posn -> polar-posn))
(define (posn->polar-posn p)
  (if (polar-posn? p) p
      (cartesian->polar p)))

(: posn+2 (posn posn -> posn))
(define (posn+2 p1 p2)
  (if (cartesian-posn? p1)
      (if (cartesian-posn? p2)
          (cartesian-posn+2 p1 p2)
          (cartesian-posn+2 p1 (polar->cartesian p2)))
      (if (cartesian-posn? p2)
          (cartesian-posn+2 (polar->cartesian p1) p2)
          (polar-posn+2 p1 p2))))

(: posn-scale (posn Number -> posn))
(define (posn-scale p c)
  (if (cartesian-posn? p)
      (cartesian-posn-scale p c)
      (polar-posn-scale p c)))

(: posn+ (posn * -> posn))
(define (posn+ . ps)
  (cond
    [(empty? ps)
     (make-cartesian-posn 0 0)]
    [(= (length ps) 1)
     (first ps)]
    [else
     (posn+2 (first ps) (apply posn+ (rest ps)))]))

(provide (all-defined-out))
