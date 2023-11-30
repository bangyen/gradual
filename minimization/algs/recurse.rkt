#lang rosette

(require "../utils/utilities.rkt")

(provide
    (contract-out
        [recurse (-> matrix?
                     (-> data?
                         bv?
                         (-> row?
                             boolean?))
                     split?)]))

(struct choice (value new old)
    #:guard (λ (value new old name)
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

    (define (inner spl val)
        (match-define
            (split in out len)
            spl)

        (case (bveq val old)
            [(#f)
             (match-define (cons fst rst) out)
             (inner (split (cons fst  in) rst len)
                    (bvor  (cdr  fst    ) val))]
            [(#t) spl]))

    (choice (inner value new)
            old old))


(define/contract (fold proc init [n 0])
    (->* ((-> natural?
              choice?
              choice?)
          choice?)
         ()
         split?)
    (if (bveq (choice-new init)
              (choice-old init))
        (choice-value init)
        (fold proc
              (proc n init)
              (add1 n))))


(define/contract (update proc)
    (-> (-> data?
            bv?
            (-> row?
                boolean?))
        (-> natural?
            choice?
            choice?))
    (define (inner n res)
        (match res
            [(choice value new old)
             (match-define
                 (split in1 out1 len)
                 value)

             (match-define
                 (split in2 out2 _)
                 (divide (proc out1 new)
                         (matrix out1
                                 len)))

             (define alt (matrix in2 len))
             (define vec (collect alt))
             (define cmb (bvor vec new))

             (if (bveq cmb old)
                 (incr (choice (split in1
                                      in2
                                      len)
                               new old))
                 (choice (split (append in2 in1)
                                out2 len)
                         cmb
                         old))]
            [else res]))

    inner)


(define (recurse mat proc)
    (define data (matrix-data mat))
    (define len  (matrix-len  mat))

    (fold (update proc)
          (choice
              (split '() data len)
              (bv 0 len)
              (collect mat))))
