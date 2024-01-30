#lang rosette

(provide collect)

(require rackunit
         racket/function
         "../pipeline/parse.rkt"
         "filter.rkt")


(define (run suite [checks '()])
    (define str
        (state checks 0))

    (match-define
        (cons
            res
            (state _ acc))
        (fold-test-results
            fold
            (cons 0 str)
            suite
            #:run (λ (_ a) a)))

    (define size
        (- acc (length checks)))

    (bv res
        (max size 1)))


(define (fold result seed)
    (match-define
        (cons
            res
            (state
                checks
                acc))
        seed)

    (define bool
        (and (not (null? checks))
             (= (car checks)
                acc)))

    (define-values
        (new tail)
        (if bool
            (values
                res (cdr checks))
            (values
                (if (test-failure?
                        (run-test-case
                            #f result))
                    (add1 (* res 2))
                    (* res 2))
                checks)))

    (cons
        new
        (state
            tail
            (add1 acc))))


(define (collect stx)
    (define (inner sub)
        (if (first
                sub
                "test-suite")
            #`#,sub
            (filter-map
                split
                sub)))

    (define (split stx)
        (branch
            stx
            inner
            #f))

    (define flat
        (flatten
            (split stx)))

    #`(test-suite "collect" #,@flat))


#; (begin
    (define checks '(1 2))

    (define suite
        (test-suite
            "outer"
            (test-suite
                "inner 1"
                (check-eq? 0 0))
            (test-suite
                "inner 2"
                (check-eq? 0 1))))

    (run suite checks))
