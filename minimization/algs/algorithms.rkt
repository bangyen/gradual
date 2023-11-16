#lang rosette

(require "../utils/utilities.rkt"
         "recurse.rkt")

(provide
    (contract-out
        [greedy (-> matrix? split?)]))

(define (mask row vec)
    (define num
        (bitvector->natural
            (bvand (bvnot vec)
                   (cdr   row))))

    (count num))

(define (greedy mat)
    (define (proc data vec)
        (define (comp row res)
            (max (mask row vec)
                 res))
`
        (define (pred row)
            (= (car row)
               (foldl comp
                      0
                      data)))

        pred)

    (recurse mat proc))
