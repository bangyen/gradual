#lang racket

(provide display-dir
         delete-dir
         wrap)
(require racket/path
         (for-syntax syntax/parse))


(define-syntax (show stx)
    (syntax-parse stx
        [(_ n t _) #'(define n t)]))

(define-syntax (hide stx)
    (syntax-parse stx
        [(_ n _ f) #'(define n f)]))


(define (delete-dir path)
    (cond
        [(file-exists? path)
         (delete-file  path)]
        [(directory-exists?
             path)
         (for-each
             delete-dir
             (directory-list
                 path
                 #:build? #t))

         (delete-directory
             path)]))


(define (copy-dir src dest ext)
    (define (copy name)
        (define file
            (build-path src name))

        (define str
            (bytes->string/utf-8
                (path-get-extension
                    name)))

        (when
            (equal? str ext)
            (copy-file
                file
                (build-path
                    dest
                    name))))

    (define files
        (directory-list
            src))

    (for-each
        copy files))


(define (copy-sub src dest sub)
    (define new-src  (build-path src  sub))
    (define new-dest (build-path dest sub))
    (make-dir new-dest)

    (copy-dir
        new-src
        new-dest
        ".rkt"))


(define (make-dir dir)
    (unless (directory-exists? dir)
        (make-directory dir)))


(define (combine
        one two
        fin config)
    (define (inner dir con)
        (define-values (q r)
            (quotient/remainder
                con 2))

        (unless (= con 1)
            (define val
                (if (= r 1)
                    one two))

            (define file (car dir))

            (hide out
                (string-replace
                    (path->string
                        (build-path
                            (path-name val)
                            file))
                    "/" "-")
                file)

            (define src  (build-path val file))
            (define dest (build-path fin out))

            (copy-file src dest)
            (inner (cdr dir) q)))

    (define files
        (directory-list
            one))

    (define len
        (length files))

    (define start
        (bitwise-ior
            (expt 2 len)
            config))

    (inner files start))


(define (setup
        src one
        two fin)
    (define (build dir)
        (build-path src dir))

    (define shared
        (build fin))

    (define (inner config)
        (delete-dir shared)
        (make-dir   shared)
        (combine
            (build one)
            (build two)
            shared
            config))

    inner)


(define (display-dir dir [level 0])
    (for ([_ level]) (display "    "))
    (displayln       (path-name  dir))

    (when (directory-exists? dir)
        (for-each
            (λ (d)
               (display-dir
                   d (add1 level)))
            (directory-list
                dir
                #:build? #t))))


(define (path-name path)
    (match-define-values
        (_ name _)
        (split-path path))

    name)


(define (wrap src fold dest)
    (delete-dir dest)
    (make-dir   dest)

    (for-each
        (λ (s)
           (copy-sub
               src dest s))
        fold)

    (setup
        dest
        "typed"
        "untyped"
        "both"
        #; "shared"))


#; (begin
    (define src  "../../../benchmarks/benchmarks/zombie/")
    (define fold '("base" "both" "untyped" "typed"))
    (define dest "config")

    (define config
        (command-line
            #:args (c)
            (string->number c)))

    (define proc
        (wrap src fold dest))

    (proc config)
    (display-dir dest)
    (delete-dir  dest))
