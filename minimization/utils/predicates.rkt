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

(define/contract (count data vec)
    (-> data? bv?
        natural?)
    (define (diff v res)
        (if (bvzero? (bvand vec v))
            res (add1 res)))

    (foldl diff 0
           (map cdr data)))


(define/contract (select mat num)
    (-> matrix?
        natural?
        bv?)
    (define data (matrix-data mat))
    (define len  (matrix-len  mat))

    (define (build v res)
        (if (= (count data v)
               num)
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
        (bveq
            (bvor (cdr v)
                  (cdr vec))
            (cdr v)))

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
