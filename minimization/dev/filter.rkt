#lang racket

(require racket/function
         "../pipeline/parse.rkt")


(struct state (checks acc))


(define (suite? stx)
    (define comp
        (compose
            negate
            curryr))

    (define inner
        (comp
            first
            "test-suite"))

    (branch stx inner #f))


(define (combine arr str func)
    (match-define
        (cons stx res)
        (foldr
            (wrap func)
            (cons '() str)
            arr))

    (cons #`#,stx res))


(define (wrap func)
    (define (inner stx res)
        (match-define
            (cons lst str)
            res)

        (match-define
            (cons new alt)
            (if (syntax->list stx)
                (func stx str)
                (cons stx str)))

        (cons
            (cons new lst)
            alt))

    inner)


(define (check stx str)
    (match-define
        (state chk acc) str)

    (match-define
        (cons  one two) chk)

    (define-values
        (arr alt)
        (if (= one acc)
            (values #'(void) two)
            (values stx      chk)))

    (define str
        (state
            alt
            (add1 acc)))

    (cons arr str))


(define (suites stx str)
    (define sub
        (syntax->list stx))

    (define comb
        (curry
            combine
            sub
            str))

    (if (and (first sub "test-suite")
             (ormap suite? sub))
        (comb check)
        (comb suites)))


(define (init stx [chk '()])
    (define lst (sort   chk   <))
    (define str (state  lst   0))
    (define res (suites stx str))

    (car res))
