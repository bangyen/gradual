#lang rosette

(require racket/struct)

(provide
    (contract-out
        [row?    contract?]
        [data?   contract?]
        [struct  matrix
                 ((data data?)
                  (len  natural?))]
        [struct  split
                 ((in  data?)
                  (out data?)
                  (len natural?))]
        [scar    (-> split? matrix?)]
        [scdr    (-> split? matrix?)]
        [divide  (-> (-> row? boolean?)
                    matrix?
                    split?)]
        [count   (-> natural?
                     natural?)]
        [pow     (-> natural?
                     (listof bv?))]
        [collect (-> matrix? bv?)]))

(define row?  (cons/c syntax? bv?))
(define data? (listof row?))


(define/contract (valid? len)
    (-> natural? contract?)
    (listof
        (cons/c
            syntax?
            (bitvector len))))


(struct matrix (data len)
    #:methods gen:custom-write
    [(define write-proc
        (make-constructor-style-printer
            (λ (obj) 'matrix)
            (λ (obj)
                (map
                    (λ (p)
                        (cons
                            (syntax->datum
                                (car p))
                            (cdr p)))
                    (matrix-data obj)))))]
    #:guard (λ (data len name)
                (unless (natural? len)
                    (error "invalid width"))
                (unless ((valid? len) data)
                    (error "invalid data"))

                (values data len)))


(struct split (in out len)
    #:guard (λ (in out len name)
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


(define (divide pred mat)
    (define data (matrix-data mat))
    (define len  (matrix-len  mat))

    (define true  (filter     pred data))
    (define false (filter-not pred data))

    (split true false len))


(define (collect mat)
    (define data (matrix-data mat))
    (define len  (matrix-len  mat))
    (define vecs (map cdr data))

    (if (null? vecs)
        (bv 0 len)
        (apply bvor
               vecs)))


(define (count num)
    (let*-values ([(q/r) quotient/remainder]
                  [(q r) (q/r num 2)])
        (if (zero? num)
            0 (+ (count q) r))))


(define (pow len)
    (define (shift n)
        (bvshl (bv 1 len)
               (bv n len)))

    (build-list len shift))
