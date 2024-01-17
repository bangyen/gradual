#lang rosette

(require racket/struct
         racket/function)

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

(define row?  (cons/c natural? bv?))
(define data? (listof row?))


(define/contract (valid? len)
    (-> natural? contract?)
    (listof
        (cons/c
            natural?
            (bitvector len))))


(define (sorted? lst)
    (and (listof natural?)
         (apply < lst)))


(struct matrix (data del len)
    #; (
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
    )
    #:guard (λ (data del len name)
                (unless (natural? len)
                    (error "invalid width"))
                (unless ((valid? len) data)
                    (error "invalid data"))
                (unless (sorted? del)
                    (error "invalid list"))

                (values data del len)))


(struct split (in out del len)
    #:guard (λ (in out len name)
                (unless (natural? len)
                    (error "invalid width"))
                (unless ((valid? len) in)
                    (error "invalid in data"))
                (unless ((valid? len) out)
                    (error "invalid out data"))
                (unless (sorted? del)
                    (error "invalid list"))

                (values in out del len)))


(define (pick func)
    (define (inner spl)
        (define res (func      spl))
        (define del (split-del spl))
        (define len (split-len spl))

        (define com
            (combine del res))

        (matrix res com len))

    inner)


(define scar (pick split-in))
(define scdr (pick split-out))


(define (combine del lst)
    (define comp
        (compose
            sort
            (curry
                append
                lst)))

    (comp del))


(define (divide pred mat [bool #t])
    (define data (matrix-data mat))
    (define del  (matrix-del  mat))
    (define len  (matrix-len  mat))

    (define true  (filter     pred data))
    (define false (filter-not pred data))

    (split true false del len))


(define (collect mat)
    (define data (matrix-data mat))
    (define len  (matrix-len  mat))
    (define vecs (map cdr data))

    (if (null? vecs)
        (bv 0 len)
        (apply bvor
               vecs)))


(define (count num)
    (let*-values
            ([(q/r) quotient/remainder]
             [(q r) (q/r num 2)])
        (if (zero? num)
            0 (+ (count q) r))))


(define (pow len)
    (define (shift n)
        (bvshl (bv 1 len)
               (bv n len)))

    (build-list len shift))
