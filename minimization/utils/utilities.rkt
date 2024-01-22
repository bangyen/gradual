#lang rosette

(require racket/struct
         racket/function)

(provide
    (contract-out
        [row?    contract?]
        [data?   contract?]
        [struct  matrix
                 ((data data?)
                  (del  sorted?)
                  (len  natural?))]
        [struct  split
                 ((in  data?)
                  (out data?)
                  (del sorted?)
                  (len natural?))]
        [pick    (->* (split?)
                      (boolean?)
                      matrix?)]
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
    (and ((listof natural?) lst)
         (or (< (length lst) 2)
             (apply < lst))))


(struct matrix (data del len)
    #:methods gen:custom-write
    [(define write-proc
        (make-constructor-style-printer
            (λ (obj) 'matrix)
            (λ (obj)
                (list
                    (matrix-del  obj)
                    (matrix-data obj)))))]
    #:guard (λ (data del len name)
                (unless (natural? len)
                    (error "invalid width"))
                (unless ((valid? len) data)
                    (error "invalid data"))
                (unless (sorted? del)
                    (error "invalid list"))

                (values data del len)))


(struct split (in out del len)
    #:guard (λ (in out del len name)
                (unless (natural? len)
                    (error "invalid width"))
                (unless ((valid? len) in)
                    (error "invalid in data"))
                (unless ((valid? len) out)
                    (error "invalid out data"))
                (unless (sorted? del)
                    (error "invalid list"))

                (values in out del len)))


(define (pick spl [first #t])
    (match-define
        (split in out del len)
        spl)

    (define res (if first in  out))
    (define alt (if first out in))
    (define num (map car alt))

    (define comp
        (compose
            (curryr
                sort <)
            (curry
                append
                num)))

    (matrix
        res
        (comp del)
        len))


(define (divide pred mat)
    (match-define
        (matrix data del len) mat)

    (define true  (filter     pred data))
    (define false (filter-not pred data))

    (split true false del len))


(define (collect mat)
    (match-define
        (matrix data _ len) mat)

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
