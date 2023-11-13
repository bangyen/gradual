#lang rosette

(require "utilities.rkt"
         "algorithms.rkt"
         rackunit)

(define/contract (matrix-eq? m1 m2)
    (-> matrix? matrix? boolean?)
    (match-define (matrix data1 len1) m1)
    (match-define (matrix data2 len2) m2)

    (define (row-eq? r1 r2)
        (match-define (cons n1 v1) r1)
        (match-define (cons n2 v2) r2)

        (and (= n1 n2)
             (bveq v1 v2)))

    (if (and (= len1 len2)
             (= (length data1)
                (length data2)))
        (andmap row-eq?
                data1
                data2)
        #f))

(define/contract (count num)
    (-> natural? natural?)
    (let*-values ([(q/r) quotient/remainder] 
                  [(q r) (q/r num 2)])
        (if (zero? num)
            0 (+ (count q) r))))

(define/contract (generate len . data)
    (->* (natural?) ()
         #:rest (listof natural?)
         matrix?)
    (define (row num)
        (cons (count num)
              (bv num len)))

    (matrix (map row data)
            len))

(define/contract (gen . data)
    (->* () ()
         #:rest (listof natural?)
         matrix?)
    (apply generate
           (cons 2 data)))


(test-case
    "greedy tests"
    (define dub (gen #b11 #b11 #b01))
    (define two (gen      #b11 #b01))
    (define one (gen           #b11))

    (define m1 (scar (greedy dub)))
    (define m2 (scar (greedy two)))
    (define m3 (scar (greedy one)))

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

    (check-true (matrix-eq? (scar spl) (gen #b01)))
    (check-true (matrix-eq? (scdr spl) (gen #b11))))

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
