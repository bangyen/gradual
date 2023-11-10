#lang rosette

(provide (all-defined-out))

(define nat?
    exact-nonnegative-integer?)

(struct matrix (num data)
    #:guard (lambda (num data name)
                (unless (nat? num)
                    (error "invalid width"))
                (unless ((listof (cons/c (and/c nat? (<=/c num))
                                         (bitvector num)))
                        data)
                    (error "invalid data"))

                (values num data)))

(define (split mat pred [ir? #f])
    (define data  (matrix-data mat))
    (define len   (matrix-num  mat))

    (define true  (filter     pred data))
    (define false (filter-not pred data))

    (values (if ir? true  (matrix len true))
            (if ir? false (matrix len false))))

(define (redundant mat)
    (lambda (vec)
        (ormap (lambda (v)
                       (if (>= (car v) (car vec))
                           (bveq (bvor (cdr v)
                                       (cdr vec))
                                 (cdr v))
                           #f))
               (matrix-data mat))))

(define (essential mat num)
    (define len  (matrix-num  mat))
    (define data (matrix-data mat))

    (define pow (build-list len
                            (lambda (n)
                                    (bvshl (bv 1 len)
                                           (bv n len)))))

    (define (count vec)
        (foldl (lambda (v res)
                       (if (bvzero? (bvand v vec))
                           res (add1 res)))
               0
               data))

    (define criteria
        (foldl (lambda (v res)
                       (if (= (count v) num)
                           (bvor v res)
                           res))
               (bv 0 len)
               pow))

    (lambda (v)
            (not (bvzero? (bvand v criteria)))))

(define (adequate new old)
    (define (all mat)
        (bvor (map cdr
                   (matrix-data mat))))

    (bveq (all new)
          (all old)))

