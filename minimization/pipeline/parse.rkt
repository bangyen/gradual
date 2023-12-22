#lang racket

(require racket/string)
(provide branch first traverse)

(read-accept-reader #t)

(define (branch stx true false)
    (define sub
        (syntax->list stx))

    (cond
        [(or (false? sub)
             (empty? sub))
         (false sub)]
        [sub
         (true  sub)]))

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

    (branch stx check (λ (s) '())))


(define/contract (open path)
    (-> path?
        (listof syntax?))
    (define file
        (open-input-file path))

    (current-input-port file)
    (define stx (read-syntax))
    (close-input-port file)

    (collect stx))


(define/contract (traverse bench)
    (-> string?
        (listof syntax?))
    (define path
        (string-append
            "../../benchmarks/"
            "benchmarks/"
            bench
            "/tests"))

    (define files
        (directory-list
            path
            #:build? #t))

    (apply
        append
        (map open
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
