#lang rosette

(provide
    (contract-out
        [row?   contract?]
        [data?  contract?]
        [struct matrix
                ((data data?)
                 (len  natural?))]
        [struct split
                ((in  data?)
                 (out data?)
                 (len natural?))]
        [scar   (-> split? matrix?)]
        [scdr   (-> split? matrix?)]
        [divide (->i ([pred (-> row? boolean?)]
                      [mat  (or/c matrix? data?)])
                     ([num  (mat)
                            (if (matrix? mat)
                                false?
                                natural?)])
                     [result split?])]
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

(define/contract (valid? len)
    (-> natural? contract?)
    (listof
        (cons/c
            (and/c natural?
                   (<=/c len))
            (bitvector len))))

(struct matrix (data len)
    #:guard (lambda (data len name)
                (unless (natural? len)
                    (error "invalid width"))
                (unless ((valid? len) data)
                    (error "invalid data"))

                (values data len)))

(struct split (in out len)
    #:guard (lambda (in out len name)
                (unless (natural? len)
                    (error "invalid width"))
                (unless ((valid? len) in)
                    (error "invalid in data"))
                (unless ((valid? len) out)
                    (error "invalid out data"))

                (values in out len)))

(define (scar spl)
    (matrix (split-in  spl)
            (split-len spl)))

(define (scdr spl)
    (matrix (split-out spl)
            (split-len spl)))

(define (divide pred mat [num #f])
    (define bool (matrix? mat))

    (define data
        (if bool
            (matrix-data mat)
            mat))

    (define len
        (if bool
            (matrix-len mat)
            num))

    (define true  (filter     pred data))
    (define false (filter-not pred data))

    (split true false len))

(define (redundant? mat)
    (define (pred vec)
        (define (count v res)
            (if (and (< res 2)
                     (>= (car v) (car vec))
                     (bveq (bvor (cdr v)
                                 (cdr vec))
                           (cdr v)))
                (add1 res)
                res))

        (define data (matrix-data mat))
        (define num  (foldl count 0 data))

        (> num 1))

    pred)

(define (essential? mat num)
    (define data (matrix-data mat))
    (define len  (matrix-len  mat))

    (define (shift n)
        (bvshl (bv 1 len)
               (bv n len)))

    (define (count v2)
        (define (diff v1 res)
            (if (bvzero? (bvand v1 v2))
                res (add1 res)))

        (foldl diff 0
               (map cdr data)))

    (define (build v res)
        (if (= (count v) num)
            (bvor v res) res))

    (define (pred v)
        (define val
            (bvand (cdr v)
                   criteria))

        (not (bvzero? val)))

    (define criteria
        (foldl build
               (bv 0 len)
               (build-list len
                           shift)))

    pred)

(define (adequate? new old)
    (define (all mat)
        (define data (matrix-data mat))
        (define len  (matrix-len  old))

        (apply bvor
               (cons (bv 0 len)
                     (map cdr
                          data))))

    (bveq (all new)
          (all old)))
