#lang scheme
(require "fake-typed.ss")

(define (new-label l)
  (symbol->string (gensym l)))

(define-struct: bulletml ([type : (U 'none 'vertical 'horizontal)]
                          [contents : (Listof element)]))

(define-type-alias element (U bullet action fire))

(define-struct: bullet ([label : String]
                        [direction : (Option direction)]
                        [speed : (Option speed)]
                        [actions : (Listof action-or-ref)]))

(define-type-alias action-or-ref (U action actionRef))

(define-struct: action ([label : String]
                        [sub-actions : (Listof sub-action)]))

(define-type-alias sub-action 
  (U repeat fire fireRef changeSpeed changeDirection accel wait vanish action-or-ref))

(define-type-alias bullet-or-ref (U bullet bulletRef))

(define-struct: fire ([label : String]
                      [direction : (Option direction)]
                      [speed : (Option speed)]
                      [bullet : bullet-or-ref]))

(define-struct: changeDirection ([direction : direction]
                                 [term : term]))

(define-struct: changeSpeed ([speed : speed]
                             [term : term]))

(define-struct: accel ([horizontal : (Option horizontal)]
                       [vertical : (Option vertical)]
                       [term : term]))

(define-struct: wait ([frames : Expression]))

(define-struct: vanish ())

(define-struct: repeat ([times : times]
                        [action : action-or-ref]))

(define-struct: direction ([type : (U 'aim 'absolute 'relative 'sequence)]
                           [degrees : Expression]))

(define-struct: speed ([type : (U 'absolute 'relative 'sequence)]
                       [units : Expression]))

(define-struct: horizontal ([type : (U 'absolute 'relative 'sequence)]
                            [units : Expression]))

(define-struct: vertical ([type : (U 'absolute 'relative 'sequence)]
                          [units : Expression]))

(define-struct: term ([how-many : Expression]))

(define-struct: times ([how-long : Expression]))

(define-struct: bulletRef ([label : String]
                           [params : (Listof param)]))

(define-struct: actionRef ([label : String]
                           [params : (Listof param)]))

(define-struct: fireRef ([label : String]
                         [params : (Listof param)]))

(define-struct: param ([value : Expression]))

(define-struct: Expression ([value-or-fun : (U Number (Number (Vectorof Number) -> Number))]))

(provide (all-defined-out))