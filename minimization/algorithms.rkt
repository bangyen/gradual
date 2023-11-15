#lang rosette

(require "utilities.rkt"
         "recurse.rkt")

(provide
    (contract-out
        [greedy (-> matrix? split?)]))

(define (greedy mat)
    (define data (matrix-data mat))
    (define bits (map car data))
    (define bset (remove-duplicates bits))

    (define (proc num)
        (define (count vec)
            (= (car vec)
               (list-ref
                   (sort bset >)
                   num)))

        count)

    (recurse mat proc))
