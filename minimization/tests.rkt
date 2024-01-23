#lang rosette

(require "utils/utilities.rkt"
         "utils/predicates.rkt"
         "algs/algorithms.rkt"
         racket/function
         rackunit)

(define/contract (matrix-eq? m1 m2)
    (-> matrix? matrix? boolean?)
    (match-define (matrix data1 del1 len1) m1)
    (match-define (matrix data2 del2 len2) m2)

    (define (row-eq? r1 r2)
        (match-define (cons n1 v1) r1)
        (match-define (cons n2 v2) r2)

        (and (= n1 n2)
             (bveq v1 v2)))

    (if (and (= len1 len2)
             (= (length data1)
                (length data2))
             (equal? del1 del2))
        (andmap row-eq?
                data1
                data2)
        #f))


(define/contract (generate data del len)
    (-> (listof
            (cons/c
                natural?
                natural?))
        (listof natural?)
        natural?
        matrix?)
    (define (row val)
        (match-define
            (cons one two)
            val)

        (cons one
              (bv two
                  len)))

    (matrix (map row
                 data)
            del len))


(define (label p f)
    (define (inner
            part full
            [in  '()]
            [out '()]
            [num   0])
        (cond
            [(null? full)
             (values
                 (reverse in)
                 (reverse out))]
            [else
             (match-define
                 (cons head1 tail1)
                 full)

             (define (next p i o)
                 (inner
                     p tail1 i o
                     (add1 num)))

             (cond
                 [(null? part)
                  (define val
                      (cons num out))

                  (next '() in val)]
                 [else
                  (match-define
                      (cons head2 tail2)
                      part)

                  (cond
                      [(= head1 head2)
                       (define new (cons num head1))
                       (define app (cons new in))
                       (next tail2 app out)]
                      [else
                       (define app (cons num out))
                       (next part  in  app)])])]))

    (inner p f))


(define/contract (gen . data)
    (->* () ()
         #:rest (listof natural?)
         matrix?)
    (define full
        '(#b11 #b11 #b10 #b01))

    (define-values
        (in out)
        (label data full))

    (generate in out 2))


(test-case
    "greedy tests"
    (define dub (gen #b11 #b11 #b01))
    (define two (gen      #b11 #b01))
    (define one (gen           #b11))

    (define m1 (pick (greedy dub)))
    (define m2 (pick (greedy two)))
    (define m3 (pick (greedy one)))

    (check-true (matrix-eq? m1 one))
    (check-true (matrix-eq? m2 one))
    (check-true (matrix-eq? m3 one)))


(test-case
    "redundant?/divide tests"
    (define mat  (gen   #b11 #b01))
    (define pred (redundant?  mat))
    (define data (matrix-data mat))
    (define spl  (divide pred mat))

    (check-false (pred (car  data)))
    (check-true  (pred (cadr data)))

    (check-true (matrix-eq? (pick spl #t) (gen #b01)))
    (check-true (matrix-eq? (pick spl #f) (gen #b11))))


(test-case
    "essential? tests"
    (define mat  (gen #b11 #b01))
    (define pred (essential?  mat 1))
    (define data (matrix-data mat))

    (check-true  (pred (car   data)))
    (check-false (pred (cadr  data))))


(test-case
    "adequate? tests"
    (define three (gen #b11 #b10 #b01))
    (define two   (gen      #b10 #b01))
    (define one   (gen           #b01))

    (check-true  (adequate? two three))
    (check-false (adequate? one two)))
