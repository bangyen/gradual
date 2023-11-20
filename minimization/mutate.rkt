#lang rosette

(require racket/stream
         syntax/parse
         mutate/mutators/code
         mutate)

(define (check p t)
    (eval #`(begin
                (require rackunit)
                (current-check-handler void)
                #,p
                (fold-test-results
                    (λ (r s)
                        (if (test-failure? r)
                            (add1 (* s 2))
                            (* s 2)))
                    0 (test-suite "" #,@t)))))

(define (split v)
    (define res
        (map
            (λ (n)
                (let*-values
                    ([(q/r) quotient/remainder]
                     [(q r) (q/r n 2)])
                    (cons q r)))
            v))

    (cons (map car res)
          (map cdr res)))

(define (conv l)
    (if (empty? l)
        0
        (+ (* 2 (conv (cdr l)))
           (car l))))

(define (build vs num)
    ; use stream instead
    (define (inner v r n)
        (cond
            [(> n 0)
             (define pair
                (split v))
             (inner
                 (car pair)
                 (cons
                     (bv (conv (cdr pair))
                         num)
                     r)
                 (sub1 n))]
            [else r]))

    (inner vs '() num))

; EXAMPLE

(define engine
    (build-mutation-engine
        #:mutators
        replace-constants/similar
        #:syntax-only
        #:streaming))

(define program
    #'(define (fact n)
          (if (> n 0)
              (* n (fact (sub1 n)))
              1)))

(define tests
    (list
        #'(check-eq? (fact 0)  1)
        #'(check-eq? (fact 1)  1)
        #'(check-eq? (fact 2)  2)
        #'(check-eq? (fact 3)  6)
        #'(check-eq? (fact 4) 24)))

(define vecs
    (map
        (λ (p)
            (check p tests))
        (stream->list
            (engine program))))

(reverse (build vecs (length tests)))
