#lang scheme

(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(define-tokens value-tokens (NUM VAR))
(define-empty-tokens op-tokens (OP CP + - * / ^ EOF NEG RAND RANK))

(define-lex-abbrevs
  (lower-letter (:/ "a" "z"))
  
  (upper-letter (:/ #\A #\Z))
  
  ;; (:/ 0 9) would not work because the lexer does not understand numbers.  (:/ #\0 #\9) is ok too.
  (digit (:/ "0" "9")))

(define calcl
  (lexer
   [(eof) 'EOF]
   ;; recursively call the lexer on the remaining input after a tab or space.  Returning the
   ;; result of that operation.  This effectively skips all whitespace.
   [(:or #\tab #\space) (calcl input-port)]
   ;; Since (token-=) returns '=, just return the symbol directly
   [(:or "+" "-" "*" "/" "^") (string->symbol lexeme)]
   ["(" 'OP]
   [")" 'CP]
   ["$rand" 'RAND]
   ["$rank" 'RANK]
   [(:: "$" (:+ digit)) (token-VAR (string->number (substring lexeme 1)))]
   [(:+ digit) (token-NUM (string->number lexeme))]
   [(:: (:+ digit) #\. (:* digit)) (token-NUM (string->number lexeme))]))

(define (lift f)
  (lambda xs
    (if (andmap number? xs)
        (apply f xs)
        (lambda (rank params)
          (apply f 
                 (map (lambda (x)
                        (if (number? x) x
                            (x rank params)))
                      xs))))))

(define-syntax-rule (define-lifted id op)
  (define (id x y)
    (if (number? x)
        (if (number? y)
            (op x y)
            (λ (rank params) (op x (y rank params))))
        (id y x))))

(define-lifted lifted-+ +)
(define-lifted lifted-- -)
(define-lifted lifted-* *)
(define-lifted lifted-/ /)
(define-lifted lifted-expt expt)

(define calcp
  (parser
   
   (start exp)
   (end EOF)
   (tokens value-tokens op-tokens)
   (error (lambda (a b c) (void)))
   
   (precs (left - +)
          (left * /)
          (left NEG)
          (right ^))
   
   (grammar
    (exp [(NUM) $1]
         [(VAR) (lambda (rank params) (vector-ref params (sub1 $1)))]
         [(RAND) (lambda (rank params) (random))]
         [(RANK) (lambda (rank params) rank)]
         [(exp + exp) (lifted-+ $1 $3)]
         [(exp - exp) (lifted-- $1 $3)]
         [(exp * exp) (lifted-* $1 $3)]
         [(exp / exp) (lifted-/ $1 $3)]
         [(- exp) (prec NEG) (lifted-- 0 $2)]
         [(exp ^ exp) (lifted-expt $1 $3)]
         [(OP exp CP) $2]))))

(define (parse-expr s)
  (define ip (open-input-string s))
  (calcp (lambda () (calcl ip))))

(provide/contract
 [parse-expr (string? . -> . (or/c number? 
                                   #;procedure?
                                   (number? (vectorof number?) . -> . number?)))])
