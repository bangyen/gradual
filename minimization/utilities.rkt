#lang rosette

(provide
       (contract-out
           [struct matrix
                   ((len  natural?)
                    (data data?))]
           [split  (or/c (->* (matrix?
                               (-> row? boolean?))
                              (#:ir? boolean?)
                              (values (or/c matrix? data?)
                                      (or/c matrix? data?))))]
           [redundant? (-> matrix?
                           (-> row? boolean?))]
           [essential? (-> matrix?
                           natural?
                           (-> row? boolean?))]
           [adequate?  (-> matrix?
                           matrix?
                           boolean?)]))

(define row?  (cons/c natural? bv?))
(define data? (listof row?))

(struct matrix (len data)
    #:guard (lambda (len data name)
                (unless (natural? len)
                    (error "invalid width"))
                (unless ((listof (cons/c (and/c natural?
                                                (<=/c len))
                                         (bitvector len)))
                        data)
                    (error "invalid data"))

                (values len data)))

(define (split mat pred #:ir? [ir? #f])
    (define data  (matrix-data mat))
    (define len   (matrix-len  mat))

    (define true  (filter     pred data))
    (define false (filter-not pred data))

    (values (if ir? true  (matrix len true))
            (if ir? false (matrix len false))))

(define (redundant? mat)
    (lambda (vec)
        (> (foldl (lambda (v res)
                          (if (>= (car v) (car vec))
                              (if (bveq (bvor (cdr v)
                                              (cdr vec))
                                        (cdr v))
                                  (add1 res)
                                  res)
                              res))
                  0 (matrix-data mat))
           1)))

(define (essential? mat num)
    (define data (matrix-data mat))
    (define len  (matrix-len  mat))
    (define one  (bv 1 len))

    (define pow (build-list len
                            (lambda (n)
                                    (bvshl one
                                           (bv n len)))))

    (define (count v2)
        (foldl (lambda (v1 res)
                       (if (bvzero? (bvand v1 v2))
                           res (add1 res)))
               0
               (map cdr
                    data)))

    (define criteria
        (foldl (lambda (v res)
                       (if (= (count v) num)
                           (bvor v res)
                           res))
               (bv 0 len)
               pow))

    (lambda (v)
            (not (bvzero? (bvand (cdr v)
                                 criteria)))))

(define (adequate? new old)
    (define (all mat)
        (apply bvor
               (map cdr
                    (matrix-data mat))))

    (bveq (all new)
          (all old)))

