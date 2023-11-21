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
    (-> (listof natural?)
        (cons/c (listof natural?)
                (listof natural?)))
    (define (get n)
        (call-with-values
            (thunk (q/r n 2))
            cons))

    (define q/r quotient/remainder)
    (define res (map get v))

    (cons (map car res)
          (map cdr res)))


(define/contract (convert rem num)
    (-> (listof natural?)
        natural?
        bv?)
    (define (inner l)
        (cond
            [(empty? l) 0]
            [else
                (match-define
                    (cons f r) l)
                (define res
                    (inner r))

                (+ f (* 2 res))]))

    (bv (inner rem) num))


(define/contract (build vs num)
    (-> (listof natural?)
        natural?
        (listof bv?))
    (define len (length vs))

    ; use stream instead
    (define (inner v r n)
        (cond
            [(> n 0)
             (match-define
                (cons a b)
                (split v))
            (define vec
                (convert b len))

             (inner a
                    (cons vec r)
                    (sub1 n))]
            [else r]))

    (inner vs '() num))


(define (pipeline muts suite)
    (define count
        (map
            (λ (p)
                (check p suite))
            (stream->list muts)))

    (define vecs
        (reverse
            (build
                count
                (length suite))))

    (define data
        (map cons
             suite
             vecs))

    (matrix data
            (length count)))
