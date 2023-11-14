#lang rosette

(require "utilities.rkt")

[provide
    (contract-out
        [essential? (-> matrix?
                        natural?
                        (-> row? boolean?))])]

(define/contract (pow len)
    (-> natural?
        (listof bv?))
    (define (shift n)
        (bvshl (bv 1 len)
               (bv n len)))

    (build-list len shift))


(define/contract (select mat num)
    (-> matrix?
        natural?
        bv?)
    (define data (matrix-data mat))
    (define len  (matrix-len  mat))
    (define vecs (map cdr data))

    (define (count v2)
        (define (diff v1 res)
            (if (bvzero? (bvand v1 v2))
                res (add1 res)))

        (foldl diff 0 vecs))

    (define (build v res)
        (if (= (count v) num)
            (bvor v res)
            res))

    (foldl build
           (bv 0 len)
           (pow len)))


(define (essential? mat num)
    (define (pred v)
        (define val
            (bvand (cdr v)
                   (select mat
                           num)))

        (not (bvzero? val)))

    pred)
