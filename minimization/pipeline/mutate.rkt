#lang rosette

(provide
    (contract-out
        [pipeline (-> (stream/c syntax?)
                      (listof syntax?)
                      matrix?)]))

(require "../utils/utilities.rkt"
         racket/stream)

(define/contract (check p t)
    (-> syntax?
        (listof syntax?)
        natural?)
    (eval
        #`(begin
            (require rackunit)
            (current-check-handler void)
            #,p
            (fold-test-results
                (λ (r s)
                    (if (test-failure? r)
                        (add1 (* s 2))
                        (* s 2)))
                0 (test-suite "" #,@t)))))


(define/contract (split v)
    (-> (stream/c natural?)
        (cons/c (stream/c natural?)
                (stream/c natural?)))
    (define (get n)
        (call-with-values
            (thunk (q/r n 2))
            cons))

    (define q/r quotient/remainder)
    (define res (stream-map get v))

    (cons (stream-map car res)
          (stream-map cdr res)))


(define/contract (convert rem)
    (-> (stream/c natural?)
        bv?)
    (define (add res next)
        (match-define
            (list old num)
            res)

        (list
            (+ next
               (* 2 old))
            (add1 num)))

    (apply bv
           (stream-fold
               add
               '(0 0)
               rem)))


(define/contract (build vs num)
    (-> (stream/c natural?)
        natural?
        (stream/c bv?))

    (define (inner v r n)
        (cond
            [(> n 0)
             (match-define
                (cons a b)
                (split v))

             (define new
                 (stream
                     (convert b)))

             (inner a
                    (stream-append
                        r new)
                    (sub1 n))]
            [else r]))

    (inner vs empty-stream num))


(define (pipeline muts suite)
    (define count
        (stream-map
            (λ (p)
                (check p suite))
            muts))

    (define vecs
            (build
                count
                (length suite)))

    (define data
        (for/list
                ([s suite]
                 [v vecs ])
            (cons s v)))

    (matrix data
            (stream-length
                count)))
