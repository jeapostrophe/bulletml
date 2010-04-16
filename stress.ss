#lang scheme
(require "parser.ss"
         "compile.ss"
         "engine.ss")

(define MAX-FRAME (* 4 (expt 10 6)))

(current-rank (random))

(define filename 
  (command-line #:program "bulletml"
                #:once-each
                [("-r" "--rank") r "Set the rank of the player"
                                 (current-rank r)]
                #:args (f) f))

(define bml (compile-bulletml (parse-file filename)))

(define height 600)
(define width 800)
(define b (load-bml width height bml))

(define (go!)
  (for ([frame (in-range MAX-FRAME)])
   (step! b)))

(require profile)
(time (profile (go!)))
