#lang scheme/gui
(require scheme/runtime-path
         "parser.ss"
         "compile.ss"
         "draw.ss")

(current-rank (random))

(define the-filename (make-parameter #f))

(command-line #:program "bulletml"
              #:once-each
              [("-r" "--rank") r "Set the rank of the player"
                               (current-rank r)]
              [("-f" "--file") f "Set the specification file"
                               (the-filename f)])

(define-runtime-path the-examples "examples")

(define filename   
  (or
   (the-filename)
   (get-file "Select a bullet description..."
             #f
             the-examples
             #f
             #f
             empty
             '(("BulletML" "*.xml")))))

(define spec (compile-bulletml (parse-file filename)))

(simulate spec)