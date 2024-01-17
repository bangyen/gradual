#lang racket

(require racket/string)
(provide branch first traverse open)

(read-accept-reader #t)

(define (branch stx func value)
    (define sub
        (syntax->list stx))

    (cond
        [(or (false? sub)
             (empty? sub))
         value]
        [sub
         (func sub)]))

(define (first sub func)
    (define id (car sub))

    (cond
        [(identifier? id)
         (define str
             (symbol->string
                 (syntax-e id)))

         (string-prefix?
             str func)]
        [else #f]))

(define/contract (collect stx)
    (-> syntax?
        (listof syntax?))
    (define (check sub)
        (if (first sub "check")
            (list stx)
            (apply
                append
                (map collect
                     sub))))

    (branch stx check '()))


(define/contract (open path)
    (-> path?
        syntax?)
    (define file
        (open-input-file path))

    (current-input-port file)
    (define stx (read-syntax))
    (close-input-port file)

    stx)


(define/contract (traverse bench)
    (-> string?
        (listof syntax?))
    (define path
        (string-append
            bench "/tests"))

    (define files
        (directory-list
            path
            #:build? #t))

    (apply
        append
        (map (λ (s)
                (collect
                    (open s)))
             files)))


#; (begin
    (define bench
        (command-line
            #:args (b) b))

    (define tests
        (traverse bench))

    (pretty-print
        (map syntax->datum
             tests)))
