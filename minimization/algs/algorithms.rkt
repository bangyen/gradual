#lang rosette

(require "../utils/utilities.rkt"
         "../utils/predicates.rkt"
         "recurse.rkt")

(provide
    (contract-out
        [greedy (-> matrix? split?)]))

(define-syntax-rule
    (pred-select expr ...)
        #`(begin
            (define
                (proc data vec)
                expr ...)
            (recurse mat proc)))

(define/contract (mask row vec)
    (-> row? bv?
        natural?)
    (define num
        (bitvector->natural
            (bvand (bvnot vec)
                   (cdr   row))))

    (count num))


(define/contract (highest data vec)
    (-> data? bv?
        (-> row?
            boolean?))
    (define (comp row res)
        (max (mask row vec)
             res))

    (define (pred row)
        (= (mask row vec)
           (foldl comp
                  0
                  data)))

    pred)


(define (greedy mat)
    (recurse mat highest))


(define (ge mat)
    (pred-select
        (if (bvzero? vec)
            (essential? mat 1)
            (highest data vec))))


(define (gre mat)
    (define new
        (scar
            (divide
                (redundant?
                    mat)
                mat)))

    (pred-select
        (if (bvzero? vec)
            (essential? mat 1)
            (highest data vec))))


(define (hgs mat)
    (define len
        (matrix-len mat))

    (pred-select
        (define (lowest v res)
            (min (count data v)
                 res))

        (define bot
            (foldl lowest 0
                   (pow len)))

        (essential? mat bot)))
