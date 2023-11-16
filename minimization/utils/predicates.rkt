#lang rosette

(require "utilities.rkt")

(provide
    (contract-out
        [essential? (-> matrix?
                        natural?
                        (-> row? boolean?))]
        [redundant? (-> matrix?
                        (-> row? boolean?))]
        [adequate?  (-> matrix?
                        matrix?
                        boolean?)]))

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


(define (redundant? mat)
    (define (sub vec v)
        (and (>= (car v) (car vec))
             (bveq (bvor (cdr v)
                         (cdr vec))
                   (cdr v))))

    (define (pred vec)
        (define (count v res)
            (if (and (< res 2)
                     (sub vec v))
                (add1 res)
                res))

        (define data (matrix-data mat))
        (define num  (foldl count 0 data))

        (> num 1))

    pred)


(define (adequate? new old)
    (bveq (collect new)
          (collect old)))
