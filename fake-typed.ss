#lang scheme

(define-syntax-rule (define-struct: struct ([field : type] ...))
  (define-struct struct (field ...)))
(define-syntax-rule (define-type-alias e ...)
  (void))
(define-syntax-rule (: f e)
  (void))

(provide (all-defined-out))