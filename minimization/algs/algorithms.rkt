#lang rosette

(provide
    (contract-out
        [greedy (-> matrix? split?)]))

(require racket/stxparam
         "../utils/utilities.rkt"
         "../utils/predicates.rkt"
         "recurse.rkt")

(define-syntax-parameter data (syntax-rules ()))
(define-syntax-parameter vec  (syntax-rules ()))
(define-syntax-parameter mat  (syntax-rules ()))

(define-syntax select
    (syntax-rules ()
        [(select expr ...)
         (begin
             (define (proc __data __vec)
                 (syntax-parameterize
                         ([data (syntax-id-rules () [_ __data])]
                          [vec  (syntax-id-rules () [_ __vec ])])
                     expr ...))
             (recurse (syntax-local-value #'mat)
                      proc))]))

(define/contract (mask row vec)
    (-> row? bv?
        natural?)
    (define num
        (bitvector->natural
            (bvand (bvnot vec)
                   (cdr   row))))

    (count num))


(define/contract (highest data vec)
    (-> data? bv?
        (-> row?
            boolean?))
    (define (comp row res)
        (max (mask row vec)
             res))

    (define (pred row)
        (= (mask row vec)
           (foldl comp
                  0
                  data)))

    pred)


(define (greedy mat)
    (recurse mat highest))


(define (ge mat)
    (select
        (if (bvzero? vec)
            (essential? mat 1)
            (highest data vec))))


(define (gre mat)
    (define new
        (scar
            (divide
                (redundant?
                    mat)
                mat)))

    (select
        (if (bvzero? vec)
            (essential? mat 1)
            (highest data vec))))


(define (hgs mat)
    (define len
        (matrix-len mat))

    (select
        (define (lowest v res)
            (min (count data v)
                 res))

        (define bot
            (foldl lowest 0
                   (pow len)))

        (essential? mat bot)))
