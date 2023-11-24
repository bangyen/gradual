#lang racket

(require racket/string)

(read-accept-reader #t)

(define (collect stx)
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

(define file
    (open-input-file
        "../../benchmarks/benchmarks/zombie/tests/test-all.rkt"))

(current-input-port file)

(define stx (read-syntax))
(pretty-print
    (map syntax->datum
         (collect stx)))

(close-input-port file)
