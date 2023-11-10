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

(define (pick mat val)
    (define (comb data)
        (cond [(empty? data) '()]
              [else (define rest
                            (comb (cdr data)))
                    (if (= (caar data) val)
                        (cons (car data) rest)
                        rest)]))

    (comb (matrix-data mat)))

(define (redundant mat)
    (define data
            (matrix-data mat))

    (define (check vec)
        (ormap (lambda (v)
                       (if (>= (car v) (car vec))
                           (bveq (bvor (cdr v)
                                       (cdr vec))
                                 (cdr v))
                           #f))
               data))

    (filter-not check data))

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

    (foldl (lambda (v res)
                   (if (= (count v) num)
                       (bvor v res)
                       res))
           (bv 0 len)
           pow))

(define (adequate new old)
    (define (all mat)
        (bvor (map cdr
                   (matrix-data mat))))

    (bveq (all new)
          (all old)))

