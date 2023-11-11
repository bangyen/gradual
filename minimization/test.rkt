#lang rosette

(require "utilities.rkt"
         rackunit)

(define/contract (matrix-eq? m1 m2)
    (-> matrix? matrix? boolean?)
    (match-let ([(matrix len1 data1) m1]
                [(matrix len2 data2) m2])
        (if (= len1 len2)
            (andmap (lambda (p1 p2)
                            (match-let ([(cons n1 v1) p1]
                                        [(cons n2 v2) p2])
                                (if (= n1 n2)
                                    (bveq v1 v2)
                                    #f)))
                    data1
                    data2)
            #f)))

(define/contract (count num)
    (-> natural? natural?)
    (let-values ([(q r)
                  (quotient/remainder num 2)])
        (if (zero? num)
            0 (+ (count q) r))))

(define/contract (generate len . data)
    (->* (natural?) ()
         #:rest (listof natural?)
         matrix?)
    (matrix len
            (map (lambda (v)
                         (cons (count v)
                               (bv v len)))
                 data)))

(define/contract (gen . data)
    (->* () ()
         #:rest (listof natural?)
         matrix?)
    (apply generate
           (cons 2 data)))


(let* ([mat  (gen   #b11 #b01)]
       [pred (redundant?  mat)]
       [data (matrix-data mat)])
    (check-false (pred (car  data)))
    (check-true  (pred (cadr data)))

    (let-values ([(r nr) (split mat pred)])
        (check-true (matrix-eq? r  (gen #b01)))
        (check-true (matrix-eq? nr (gen #b11)))))

(let* ([mat  (gen #b11 #b01)]
       [pred (essential?  mat 1)]
       [data (matrix-data mat)])
    (check-true  (pred (car   data)))
    (check-false (pred (cadr  data))))

(let ([three (gen #b11 #b10 #b01)]
      [two   (gen      #b10 #b01)]
      [one   (gen           #b01)])
    (check-true  (adequate? two three))
    (check-false (adequate? one two)))
