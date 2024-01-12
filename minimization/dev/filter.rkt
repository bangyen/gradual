#lang racket

(require "../pipeline/parse.rkt")

(define (update pair head [tail #f])
    (match-define
        (cons lst acc)
        pair)

    (define new
        (if (false? tail)
            acc tail))

    (cons (cons head lst)
          new))


(define (combine arr func)
    (define res
        (foldl
            func
            (cons '() acc)
            (reverse arr)))

    (cons #`#,(car res)
          (cdr res)))


(define (check new res checks)
    (match-define
        (cons lst cur) res)

    (define app
        (if (ormap
                (=/c cur)
                checks)
            (cons new lst)
            lst))

    (cons app
          (add1 cur)))


(define (suite? stx)
    (define (inner arr)
        (not
            (first
                arr
                "test-suite")))

    (branch stx
            inner
            #f))


(define (suites sub acc checks)
    (define (unwrap stx res)
        (lists stx
               (cdr res)
               checks))

    (define (select stx res)
        (define (inner new)
            (check new res checks))

        (branch
            stx inner
            (update res stx)))

    (if (and (first sub "test-suite")
             (ormap suite? sub))
        (combine sub select)
        (combine sub unwrap)))


(define (lists stx acc checks)
    (define (mutual s)
        (suites s acc checks))

    (define (unwrap stx res)
        (match-define
            (cons new inc)
            (branch stx
                    mutual
                    stx))

        (update res new inc))

    unwrap)
