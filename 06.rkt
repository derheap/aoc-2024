#lang racket
(require rackunit)
(require racket/hash)

(define input
  (file->lines "input/06.txt"))
(define test_input
  (string-split
   (string-trim "2333133121414131402"
                #px"\\s+"
                #:repeat? #t)
   "\n"))

(define (grid-size-y grid)
  (length grid))
(define (grid-size-x grid)
  (string-length (first grid)))

;
; Convert an ASCII grid to an hash.
; The keys are the x,y coordinates.
;
(define (save-pos? k)
  (if (member k '(#\# #\^)) #t #f))
(check-equal? (save-pos? #\#) #t)
(check-equal? (save-pos? #\^) #t)
(check-equal? (save-pos? #\.) #f)

(define (map-grid ht grid x y)
  (let ([hd (first grid)]
        [tl (rest grid)])
    (map-line ht (string->list hd) 0 y)
    (if (empty? tl)
        ht
        (map-grid ht tl 0 (+ y 1)))))

; Convert a single line.
(define (map-line ht line x y)
  (let ([hd (first line)]
        [tl (rest line)])
    (if (save-pos? hd)
        (hash-set!
         ht
         (string-append-immutable
          (number->string x)
          ","
          (number->string y))
         hd)
        null)
    (if (empty? tl)
        ht
        (map-line ht tl (+ x 1) y))))

; Get item from grid. Undefined positions return '.'.
(define (at ht x y)
  (hash-ref ht
            (string-append-immutable
             (number->string x)
             ","
             (number->string y))
            #\.))

(define (find-guard-start ht)
  (map string->number
       (string-split
        (first (hash-keys
                (hash-filter-values
                 ht
                 (λ (v)
                   (equal? v #\^)))))
        ",")))

(define (on-grid? xmax ymax x y)
  (and (>= x 0)
       (< x xmax)
       (>= y 0)
       (< y ymax)))
(check-equal? (on-grid? 10 10 0 0) #t)
(check-equal? (on-grid? 10 10 -1 0) #f)
(check-equal? (on-grid? 10 10 0 -1) #f)
(check-equal? (on-grid? 10 10 10 0) #f)
(check-equal? (on-grid? 10 10 0 10) #f)

(define (is-obstacle? grid x y)
  (equal? (at grid x y) #\#))

(define (next-pos-x dir x)
  (cond
    [(equal? dir #\N) x]
    [(equal? dir #\E) (+ x 1)]
    [(equal? dir #\S) x]
    [(equal? dir #\W) (- x 1)]))

(define (next-pos-y dir y)
  (cond
    [(equal? dir #\N) (- y 1)]
    [(equal? dir #\E) y]
    [(equal? dir #\S) (+ y 1)]
    [(equal? dir #\W) y]))

(define (is-obstacle-next-step grid
                               dir
                               x
                               y)
  (is-obstacle? grid
                (next-pos-x dir x)
                (next-pos-y dir y)))

(define (turn-right dir)
  (cond
    [(equal? dir #\N) #\E]
    [(equal? dir #\E) #\S]
    [(equal? dir #\S) #\W]
    [(equal? dir #\W) #\N]))

(define (next-step grid
                   locations
                   xmax
                   ymax
                   dir
                   x
                   y)
  (hash-set! locations
             (string-append-immutable
              (number->string x)
              ","
              (number->string y))
             dir)
  (if (is-obstacle-next-step grid
                             dir
                             x
                             y)
      (step grid
            locations
            xmax
            ymax
            (turn-right dir)
            x
            y)
      (step grid
            locations
            xmax
            ymax
            dir
            (next-pos-x dir x)
            (next-pos-y dir y))))

(define (step grid
              locations
              xmax
              ymax
              dir
              x
              y)
  (if (on-grid? xmax ymax x y)
      (next-step grid
                 locations
                 xmax
                 ymax
                 dir
                 x
                 y)
      locations))

; Right on first try!
(define (part#1 input)
  (define grid (make-hash))
  (define locations (make-hash))
  (define xmax (grid-size-x input))
  (define ymax (grid-size-y input))
  (map-grid grid input 0 0)
  (define gs (find-guard-start grid))
  (define x (first gs))
  (define y (first (rest gs)))
  (length (hash-values (step grid
                             locations
                             xmax
                             ymax
                             #\N
                             x
                             y))))

(check-equal? (part#1 test_input) 41)
(part#1 input)

(define (is-empty? ht x y)
  (equal? (at x y) #\.))

(define (is-looping? ht dir x y)
  (hash-has-key?
   ht
   (string-append-immutable
    (number->string x)
    ","
    (number->string y)
    "@"
    (string dir))))

(define (next-step-p2 grid
                      locations
                      xmax
                      ymax
                      dir
                      x
                      y)
  (hash-set! locations
             (string-append-immutable
              (number->string x)
              ","
              (number->string y)
              "@"
              (string dir))
             dir)
  (if (is-obstacle-next-step grid
                             dir
                             x
                             y)
      (step-p2 grid
               locations
               xmax
               ymax
               (turn-right dir)
               x
               y)
      (step-p2 grid
               locations
               xmax
               ymax
               dir
               (next-pos-x dir x)
               (next-pos-y dir y))))

(define (step-p2 grid
                 locations
                 xmax
                 ymax
                 dir
                 x
                 y)
  (cond
    [(is-looping? locations dir x y) #t]
    [(on-grid? xmax ymax x y)
     (next-step-p2 grid
                   locations
                   xmax
                   ymax
                   dir
                   x
                   y)]
    [else #f]))

(define (add-obstacle ht x y)
  (hash-set! ht
             (string-append-immutable
              (number->string x)
              ","
              (number->string y))
             #\#))

(define (add-obstacle-and-test-looping
         grid
         x
         y
         xmax
         ymax
         gx
         gy)
  (define locations (make-hash))
  (define test-grid (hash-copy grid))
  (add-obstacle test-grid x y)
  (if (step-p2 test-grid
               locations
               xmax
               ymax
               #\N
               gx
               gy)
      1
      0))

(define (part#2 input)
  (define grid (make-hash))
  (define path-hash (make-hash))
  (define xmax (grid-size-x input))
  (define ymax (grid-size-y input))
  (map-grid grid input 0 0)
  (define gs (find-guard-start grid))
  (define gx (first gs))
  (define gy (first (rest gs)))
  (apply
   +
   (for/list ([y (range ymax)])
     (apply
      +
      (for/list ([x (range xmax)])
        (add-obstacle-and-test-looping
         grid
         x
         y
         xmax
         ymax
         gx
         gy))))))

(check-equal? (part#2 test_input) 6)
(part#2 input)
