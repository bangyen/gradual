#lang racket

(provide start (struct-out state))

(require racket/function
         "../pipeline/parse.rkt")


(struct state (checks acc))


(define (func? fn)
    (define (inner stx)
        (define comp
            (compose
                negate
                curryr))

        (define inner
            (comp
                first
                fn))

        (branch stx inner #f))
    inner)


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

    (define bool
        (and (not (null? chk))
             (= (car chk) acc)))

    (define-values
        (arr alt)
        (if bool
            (values #'(void) (cdr chk))
            (values stx      chk)))

    (define new
        (state
            alt
            (add1 acc)))

    (cons arr new))


(define (suites stx str)
    (define sub (syntax->list stx))
    (define ts  "test-suite")
    (define fn  (func? ts))

    (define comb
        (curry
            combine
            sub
            str))

    (if (and (first sub ts)
             (ormap fn sub))
        (comb check)
        (comb suites)))


(define (start stx [chk '()])
    (define lst (sort   chk   <))
    (define str (state  lst   0))
    (define res (suites stx str))

    (car res))
