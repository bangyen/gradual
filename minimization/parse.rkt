#lang racket

(require racket/string)

(read-accept-reader #t)

(define/contract (collect stx)
    (-> syntax?
        (listof syntax?))
    (define sub
        (syntax->list stx))

    (cond
        [(or (false? sub)
             (empty? sub))
         '()]
        [sub
         (define id (car sub))

         (define res
             (cond
                 [(identifier? id)
                  (define str
                      (symbol->string
                          (syntax-e id)))

                  (string-contains?
                      str "check")]
                 [else #f]))

         (cond
             [res (list stx)]
             [else
              (apply
                  append
                  (map collect
                       sub))])]))


(define/contract (open path)
    (-> path?
        (listof syntax?))
    (define file
        (open-input-file path))

    (current-input-port file)

    (define stx (read-syntax))
    (define res (collect stx))

    (close-input-port file)

    res)


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


(define bench
    (command-line
        #:args (b) b))

(define tests
    (traverse bench))

(length tests)
#; (pretty-print
    (map syntax->datum
         tests))
