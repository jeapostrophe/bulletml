#lang scheme
(require "fake-typed.ss")

; XXX Add term language

(define-struct: bulletdl ([type : (U 'vertical 'horizontal)]
                          [top : (Option action)]))

; XXX Add data
(define-struct: bullet ([direction : (Option direction)]
                        [speed : (Option speed)]
                        [params : (Vectorof Expression)]
                        [action : (Option action)]))

(define-type-alias action (U seqn repeat fire changeSpeed changeDirection accel wait vanish))

(define-struct: seqn ([actions : (Listof action)]))

; XXX Add data
; XXX Add firing many bullets
(define-struct: fire ([direction : (Option direction)]
                      [speed : (Option speed)]
                      [bullet : bullet]))

(define-struct: changeDirection ([direction : direction]
                                 [frames : Expression]))

(define-struct: changeSpeed ([speed : speed]
                             [frames : Expression]))

(define-struct: accel ([horizontal : (Option horizontal)]
                       [vertical : (Option vertical)]
                       [frames : Expression]))

(define-struct: wait ([frames : Expression]))

(define-struct: vanish ())

(define-struct: repeat ([times : Expression]
                        [action : action]))

(define-struct: direction ([type : (U 'aim 'absolute 'relative 'sequence)]
                           [degrees : Expression]))

(define-struct: speed ([type : (U 'absolute 'relative 'sequence)]
                       [units : Expression]))

(define-struct: horizontal ([type : (U 'absolute 'relative 'sequence)]
                            [units : Expression]))

(define-struct: vertical ([type : (U 'absolute 'relative 'sequence)]
                          [units : Expression]))

(define-struct: stalled-params ([evector : (Vectorof Expression)]
                                [inner : action]))

(define-struct: saved-params ([pvector : (Vectorof Number)]
                              [inner : action]))

(define-struct: Expression ([value-or-fun : (U Number 
                                               (Number (Vectorof Number) -> Number))]))

(provide (all-defined-out))