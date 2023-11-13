#lang rosette

(require "utilities.rkt")

(provide
    (contract-out
        [greedy (-> matrix? split?)]))

(struct choice (value new old)
    #:guard (lambda (value new old name)
                (unless (split? value)
                    (error "invalid split"))
                (unless (bv? new)
                    (error "invalid new bv"))
                (unless (bv? old)
                    (error "invalid old bv"))

                (values value new old)))

(define/contract (incr chc)
    (-> choice? choice?)
    (match-define
        (choice value new old)
        chc)

    (match-define
        (split in1 out1 len)
        value)

    (case (bveq new old)
        [(#f) (match-define (cons first rest) out1)
              (define  in2  (cons first in1))
              (define out2  rest)
              (incr (choice (split in2 out2 len)
                            (bvor (cdr first)
                                  new)
                            old))]
        [(#t) chc]))

(define/contract (fold proc init [n 0])
    (->* ((-> natural? choice? choice?)
          choice?)
         ()
         split?)
    (if (bveq (choice-new init)
              (choice-old init))
        (choice-value init)
        (fold proc
              (proc n init)
              (add1 n))))

(define/contract (recurse mat proc)
    (-> matrix?
        (-> natural?
            (-> row?
                boolean?))
        split?)
    (define data (matrix-data mat))
    (define len  (matrix-len  mat))
    (define col  (collect     mat))

    (define (update n res)
        (match res
            [(choice value new old)
             (match-define
                 (split in1 out1 _l1)
                 value)

             (match-define
                 (split in2 out2 _l2)
                 (divide (proc n)
                         out1
                         len))

             (define comb (append in2 in1))
             (define new  (matrix comb len))
             (define alt  (collect new))

             (if (bveq alt col)
                 (incr (choice (split in1
                                      in2
                                      len)
                               (collect (matrix in1
                                                len))
                               col))
                 (choice (split comb out2 len)
                         alt
                         col))]
            [else res]))

    (fold update
          (choice
              (split '()
                     data
                     len)
              (bv 0 len)
              col)))

(define (greedy mat)
    (define data (matrix-data mat))
    (define bits (map car data))
    (define bset (remove-duplicates bits))

    (define (proc num)
        (define (count vec)
            (= (car vec)
               (list-ref
                   (sort bset >)
                   num)))

        count)

    (recurse mat proc))
