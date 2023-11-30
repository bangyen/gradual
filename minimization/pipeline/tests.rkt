#lang rosette

(require "mutate.rkt"
         mutate/mutators/code
         mutate
         rackunit)

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

(pretty-print
    (pipeline
        (engine program)
        tests))
