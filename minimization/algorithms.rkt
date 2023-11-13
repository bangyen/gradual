#lang rosette

(require "utilities.rkt")

(provide
    (contract-out
        [greedy (-> matrix? split?)]))

(struct choice (value done)
    #:guard (lambda (value done name)
                (unless (split? value)
                    (error "invalid split"))
                (unless (boolean? done)
                    (error "invalid condition"))

                (values value done)))

(define/contract (increment next res mat)
    (-> data? data? matrix? split?)
    (define len (matrix-len mat))
    (define old (matrix res len))

    (case (adequate? old mat)
        [(#f) (increment (cdr next)
                         (cons (car next)
                               res)
                         mat)]
        [(#t) (split res next len)]))

(define/contract (fold proc init [n 0])
    (->* ((-> natural? choice? choice?)
          choice?)
         ()
         split?)
    (if (choice-done  init)
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

    (define (update n res)
        (match res
            [(choice value done)
             (match-define
                 (split in out __)
                 value)

             (define next
                 (divide (proc n)
                         out
                         len))

             (match-let ([(split t f _) next])
                 (define comb (append t in))
                 (define new  (matrix comb len))

                 (if (adequate? new mat)
                     (let ([spl (increment t
                                           in
                                           mat)])
                         (choice spl #t))
                     (choice (split comb f len)
                             #f)))]
            [else res]))

    (fold update
          (choice
              (split '()
                     data
                     len)
              #f)))

(define (greedy mat)
    (define sizes
        (sort
            (remove-duplicates
                (map car (matrix-data mat)))
            >))

    (define proc
        (lambda (num)
                (lambda (vec)
                        (= (car vec)
                           (list-ref sizes
                                     num)))))

    (recurse mat proc))
