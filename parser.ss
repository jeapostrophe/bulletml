#lang scheme
(require (only-in xml
                  xml->xexpr
                  document-element
                  read-xml
                  collapse-whitespace
                  xexpr-drop-empty-attributes)
         "expr.ss"
         "ml.ss")

(define (parse-file pth)
  (parse-xexpr
   (with-input-from-file pth
     (lambda () 
       (parameterize ([collapse-whitespace #t]
                      [xexpr-drop-empty-attributes #t])
         (xml->xexpr (document-element (read-xml))))))))

(define (parse-xexpr xe)
  (parse-bulletml xe))

(provide/contract
 [parse-file (path-string? . -> . bulletml?)])

;;;; Details
(define parse-bulletml
  (match-lambda
    [`(bulletml ((xmlns "http://www.asahi-net.or.jp/~cs8k-cyu/bulletml")) . ,elements)
     (parse-bulletml
      `(bulletml ((type "none") (xmlns "http://www.asahi-net.or.jp/~cs8k-cyu/bulletml")) . ,elements))]
    [`(bulletml ((type ,type) (xmlns "http://www.asahi-net.or.jp/~cs8k-cyu/bulletml")) . ,elements)
     (make-bulletml (string->symbol type)
                    (map parse-element (filter-not string? elements)))]))

(define-syntax-rule (define-parser id [tag parser] ...)
  (define (id xe)
    (match xe
      [`(tag . ,rest) (parser xe)]
      ...)))

(define-parser parse-element
  [action parse-action]
  [bullet parse-bullet]
  [fire parse-fire])

(define parse-bullet
  (match-lambda
    [`(bullet ((label ,label)) . ,bullet-contents)
     (define contents
       (map parse-bullet-contents (filter-not string? bullet-contents)))
     (define maybe-direction (findf direction? contents))
     (define maybe-speed (findf speed? contents))
     (define actions (filter-not (lambda (x) (or (direction? x) (speed? x))) contents))
     (make-bullet label maybe-direction maybe-speed actions)]
    [`(bullet . ,bullet-contents)
     (parse-bullet `(bullet ((label ,(new-label 'bullet))) . ,bullet-contents))]))

(define-parser parse-bullet-contents
  [direction parse-direction]
  [speed parse-speed]
  [action parse-action]
  [actionRef parse-actionRef])

(define parse-action
  (match-lambda
    [`(action ((label ,label)) . ,action-contents)
     (make-action label (map parse-action-contents (filter-not string? action-contents)))]
    [`(action . ,action-contents)
     (parse-action `(action ((label ,(new-label 'action))) . ,action-contents))]))

(define-parser parse-action-contents
  [repeat parse-repeat]
  [fire parse-fire]
  [fireRef parse-fireRef]
  [changeSpeed parse-changeSpeed]
  [changeDirection parse-changeDirection]
  [accel parse-accel]
  [wait parse-wait]
  [vanish parse-vanish]
  [action parse-action]
  [actionRef parse-actionRef])

(define parse-fire
  (match-lambda
    [`(fire ((label ,label)) . ,fire-contents)
     (define contents
       (map parse-fire-contents (filter-not string? fire-contents)))
     (define maybe-direction (findf direction? contents))
     (define maybe-speed (findf speed? contents))
     (define actions (filter-not (lambda (x) (or (direction? x) (speed? x))) contents))
     (make-fire label maybe-direction maybe-speed (first actions))]
    [`(fire . ,fire-contents)
     (parse-fire `(fire ([label ,(new-label 'fire)]) . ,fire-contents))]))

(define-parser parse-fire-contents
  [direction parse-direction]
  [speed parse-speed]
  [bullet parse-bullet]
  [bulletRef parse-bulletRef])

(define parse-changeDirection
  (match-lambda
    [`(changeDirection ,@(list-no-order (and direction `(direction . ,drest)) (and term `(term . ,trest)) " " ...))
     (make-changeDirection (parse-direction direction) (parse-term term))]))

(define parse-changeSpeed
  (match-lambda
    [`(changeSpeed ,@(list-no-order (and speed `(speed . ,drest)) (and term `(term . ,trest)) " " ...))
     (make-changeSpeed (parse-speed speed) (parse-term term))]))

(define parse-accel
  (match-lambda
    [`(accel . ,fire-contents)
     (define contents
       (map parse-accel-contents (filter-not string? fire-contents)))
     (define maybe-horizontal (findf horizontal? contents))
     (define maybe-vertical (findf vertical? contents))
     (define term (findf term? contents))
     (make-accel maybe-horizontal maybe-vertical term)]))

(define-parser parse-accel-contents
  [horizontal parse-horizontal]
  [vertical parse-vertical]
  [term parse-term])

(define parse-wait
  (match-lambda
    [`(wait ,expr)
     (make-wait (parse-NUMBER expr))]))

(define parse-vanish 
  (match-lambda
    [`(vanish)
     (make-vanish)]))

(define parse-repeat
  (match-lambda
    [`(repeat ,@(list-no-order (and one `(times . ,drest)) (and two `(action . ,trest)) " " ...))
     (make-repeat (parse-times one) (parse-action two))]
    [`(repeat ,@(list-no-order (and one `(times . ,drest)) (and two `(actionRef . ,trest)) " " ...))
     (make-repeat (parse-times one) (parse-actionRef two))]))

(define parse-direction
  (match-lambda
    [`(direction ([type ,type]) ,contents)
     (make-direction (string->symbol type) (parse-NUMBER contents))]
    [`(direction ,contents)
     (make-direction 'aim (parse-NUMBER contents))]))
(define parse-speed
  (match-lambda
    [`(speed ([type ,type]) ,contents)
     (make-speed (string->symbol type) (parse-NUMBER contents))]
    [`(speed ,contents)
     (make-speed 'absolute (parse-NUMBER contents))]))
(define parse-horizontal
  (match-lambda
    [`(horizontal ([type ,type]) ,contents)
     (make-horizontal (string->symbol type) (parse-NUMBER contents))]
    [`(horizontal ,contents)
     (make-horizontal 'absolute (parse-NUMBER contents))]))
(define parse-vertical
  (match-lambda
    [`(vertical ([type ,type]) ,contents)
     (make-vertical (string->symbol type) (parse-NUMBER contents))]
    [`(vertical ,contents)
     (make-vertical 'absolute (parse-NUMBER contents))]))

(define parse-term
  (match-lambda
    [`(term ,contents)
     (make-term (parse-NUMBER contents))]))
(define parse-times
  (match-lambda
    [`(times ,contents)
     (make-times (parse-NUMBER contents))]))

(define-syntax-rule (define-parse-ref id tag make)
  (define id
    (match-lambda
      [`(tag ([label ,label]) . ,contents)
       (make label (map parse-param (filter-not string? contents)))])))

(define-parse-ref parse-bulletRef bulletRef make-bulletRef)
(define-parse-ref parse-actionRef actionRef make-actionRef)
(define-parse-ref parse-fireRef fireRef make-fireRef)

(define parse-param
  (match-lambda
    [`(param ,contents)
     (make-param (parse-NUMBER contents))]))

(define (parse-NUMBER e)
  (make-Expression (parse-expr e)))