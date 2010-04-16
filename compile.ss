#lang scheme
(require (prefix-in ml: "ml.ss")
         (prefix-in dl: "dl.ss"))

(define top-label "top")

(define E->E
  (match-lambda
    [(struct ml:Expression (v))
     (dl:make-Expression v)]))

(define-syntax-rule (define-compile id ml:s1 dl:make-s1)
  (define id
    (match-lambda
      [#f #f]
      [(struct ml:s1 (type degs))
       (dl:make-s1 type (E->E degs))])))

(define-compile ml:direction->dl:direction ml:direction dl:make-direction)
(define-compile ml:speed->dl:speed ml:speed dl:make-speed)
(define-compile ml:horizontal->dl:horizontal ml:horizontal dl:make-horizontal)
(define-compile ml:vertical->dl:vertical ml:vertical dl:make-vertical)

(define (dl:make-seqn* as)
  (define fas (filter (lambda (x) x) as))
  (match fas
    [(list) #f]
    [(list a) a]
    [as
     (dl:make-seqn as)]))

(define (dl:make-stalled-params* p i)
  (if i
      (dl:make-stalled-params p i)
      #f))

(define (dl:make-repeat* t a)
  (if a
      (dl:make-repeat t a)
      #f))

(define (compile-bulletml bml)
  (define ml:bullets (make-hash))
  (define ml:actions (make-hash))
  (define ml:fires (make-hash))
  
  (define dl:bullets (make-hash))
  (define dl:actions (make-hash))
  (define dl:fires (make-hash))
  
  (define (lookup-action l)
    (hash-ref! dl:actions l
               (lambda ()
                 (ml:action->dl:action (hash-ref ml:actions l)))))
  (define (lookup-fire l)
    (hash-ref! dl:fires l
               (lambda ()
                 (ml:fire->dl:fire (hash-ref ml:fires l)))))
  (define (lookup-bullet l)
    (hash-ref! dl:bullets l
               (lambda ()
                 (ml:bullet->dl:bullet (hash-ref ml:bullets l)))))
  
  (define ml:fire->dl:fire
    (match-lambda
      [(struct ml:fire (_ dir speed bullet))
       (dl:make-fire (ml:direction->dl:direction dir)
                     (ml:speed->dl:speed speed)
                     (ml:bullet-or-ref->dl:bullet bullet))]))
  (define ml:bullet->dl:bullet
    (match-lambda
      [(struct ml:bullet (_ dir speed acts))
       (dl:make-bullet (ml:direction->dl:direction dir)
                       (ml:speed->dl:speed speed)
                       (vector)
                       (dl:make-seqn*
                        (map ml:action->dl:action acts)))]))
  (define ml:bullet-or-ref->dl:bullet
    (match-lambda
      [(? ml:bullet? b)
       (ml:bullet->dl:bullet b)]
      [(struct ml:bulletRef (label ml:params))
       (define dl:params 
         (list->vector (map (compose E->E ml:param-value) ml:params)))
       (define dl:bul
         (lookup-bullet label))
       
       (struct-copy dl:bullet dl:bul
                    [params dl:params])]))
  (define ml:action->dl:action
    (match-lambda
      [(struct ml:repeat (times a))
       (dl:make-repeat* (E->E (ml:times-how-long times))
                        (ml:action->dl:action a))]
      [(? ml:fire? f)
       (ml:fire->dl:fire f)]
      [(struct ml:fireRef (label params))
       (dl:make-stalled-params
        (list->vector (map (compose E->E ml:param-value) params))
        (lookup-fire label))]
      [(struct ml:changeSpeed (speed term))
       (dl:make-changeSpeed (ml:speed->dl:speed speed)
                            (E->E (ml:term-how-many term)))]
      [(struct ml:changeDirection (dir term))
       (dl:make-changeDirection (ml:direction->dl:direction dir)
                                (E->E (ml:term-how-many term)))]
      [(struct ml:accel (hor vert term))
       (dl:make-accel (ml:horizontal->dl:horizontal hor)
                      (ml:vertical->dl:vertical vert)
                      (E->E (ml:term-how-many term)))]
      [(struct ml:wait (frames))
       (dl:make-wait (E->E frames))]
      [(struct ml:vanish ())
       (dl:make-vanish)]
      [(struct ml:action (_ sa))
       (dl:make-seqn* (map ml:action->dl:action sa))]
      [(struct ml:actionRef (label params))
       (dl:make-stalled-params*
        (list->vector (map (compose E->E ml:param-value) params))
        (lookup-action label))]))
  
  (define type (ml:bulletml-type bml))
  
  (for ([e (in-list (ml:bulletml-contents bml))])
    (match e
      [(? ml:bullet?)
       (hash-set! ml:bullets (ml:bullet-label e) e)]
      [(? ml:action?)
       (hash-set! ml:actions (ml:action-label e) e)]
      [(? ml:fire?)
       (hash-set! ml:fires (ml:fire-label e) e)]))
  
  (dl:make-bulletdl (case type
                      [(none) 'vertical]
                      [else type])
                    (lookup-action top-label)))

(provide/contract
 [compile-bulletml (ml:bulletml? . -> . dl:bulletdl?)])