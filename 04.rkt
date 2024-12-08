#lang racket
(require rackunit)

(define input (file->lines "input/04.txt"))
(define test_input
  (string-split
   (string-trim
    "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX\n\n"
    #px"\\s+"
    #:repeat? #t)
   "\n"))

(define (split-line line)
  (string-split line #px"\\s+"))
(define (split-lines line)
  (map split-line line))

(define (grid-size-y grid)
  (length grid))
(define (grid-size-x grid)
  (string-length (first grid)))

;
; Convert an ASCII grid to an hash.
; The keys are the x,y coordinates.
;
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
    (hash-set!
     ht
     (string-append-immutable (number->string x)
                              ","
                              (number->string y))
     hd)
    (if (empty? tl)
        ht
        (map-line ht tl (+ x 1) y))))

; Get item from grid. Undefined positions return '.'.
(define (at ht x y)
  (hash-ref
   ht
   (string-append-immutable (number->string x)
                            ","
                            (number->string y))
   #\.))
;
; Look for XMAS in all directions at x,y.
;
(define (neighbours-lr ht x y)
  (list (at ht (+ x 0) y)
        (at ht (+ x 1) y)
        (at ht (+ x 2) y)
        (at ht (+ x 3) y)))
(define (neighbours-rl ht x y)
  (list (at ht (- x 0) y)
        (at ht (- x 1) y)
        (at ht (- x 2) y)
        (at ht (- x 3) y)))
(define (neighbours-tb ht x y)
  (list (at ht (+ x 0) (+ y 0))
        (at ht (+ x 0) (+ y 1))
        (at ht (+ x 0) (+ y 2))
        (at ht (+ x 0) (+ y 3))))
(define (neighbours-bt ht x y)
  (list (at ht (+ x 0) (- y 0))
        (at ht (+ x 0) (- y 1))
        (at ht (+ x 0) (- y 2))
        (at ht (+ x 0) (- y 3))))
(define (neighbours-lrtb ht x y)
  (list (at ht (+ x 0) (+ y 0))
        (at ht (+ x 1) (+ y 1))
        (at ht (+ x 2) (+ y 2))
        (at ht (+ x 3) (+ y 3))))
(define (neighbours-lrbt ht x y)
  (list (at ht (+ x 0) (- y 0))
        (at ht (+ x 1) (- y 1))
        (at ht (+ x 2) (- y 2))
        (at ht (+ x 3) (- y 3))))
(define (neighbours-rltb ht x y)
  (list (at ht (- x 0) (+ y 0))
        (at ht (- x 1) (+ y 1))
        (at ht (- x 2) (+ y 2))
        (at ht (- x 3) (+ y 3))))
(define (neighbours-rlbt ht x y)
  (list (at ht (- x 0) (- y 0))
        (at ht (- x 1) (- y 1))
        (at ht (- x 2) (- y 2))
        (at ht (- x 3) (- y 3))))

(define (is-xmas? l)
  (equal? l (list #\X #\M #\A #\S)))
(define (is-xmas-count l)
  (if (is-xmas? l) 1 0))
(check-equal? (is-xmas? (list #\X #\M #\A #\S))
              #t)
(check-equal? (is-xmas? (list #\X #\X #\A #\S))
              #f)

; Count found XMAS in all directions, at x,y.
(define (count-xmax-at ht x y)
  (+ (is-xmas-count (neighbours-rl ht x y))
     (is-xmas-count (neighbours-lr ht x y))
     (is-xmas-count (neighbours-tb ht x y))
     (is-xmas-count (neighbours-bt ht x y))
     (is-xmas-count (neighbours-lrtb ht x y))
     (is-xmas-count (neighbours-lrbt ht x y))
     (is-xmas-count (neighbours-rltb ht x y))
     (is-xmas-count (neighbours-rlbt ht x y))))

;
; Make grid and iterate over grid,
; couting XMAS in any direction from a point.
;
(define (part#1 input)
  (define ht (make-hash))
  (define xmax (grid-size-x input))
  (define ymax (grid-size-y input))
  (map-grid ht input 0 0)

  (apply +
         (for/list ([y (range ymax)])
           (apply +
                  (for/list ([x (range xmax)])
                    (count-xmax-at ht x y))))))

(check-equal? (part#1 test_input) 18)
(part#1 input)

(define (xmax-lrtb ht x y)
  (list (at ht (- x 1) (- y 1))
        (at ht (- x 0) (- y 0))
        (at ht (+ x 1) (+ y 1))
        ))

(define (xmax-rltb ht x y)
  (list (at ht (+ x 1) (- y 1))
        (at ht (- x 0) (- y 0))
        (at ht (- x 1) (+ y 1))
        ))

(define (is-x-mas? l)
  (or (equal? l (list #\M #\A #\S)) (equal? l (list #\S #\A #\M))))

(define (count-x-mas-at ht x y)
  (if (and
      (is-x-mas? (xmax-lrtb ht x y))
      (is-x-mas? (xmax-rltb ht x y)))
      1 0))

(define (part#2 input)
    (define ht (make-hash))
    (define xmax (grid-size-x input))
    (define ymax (grid-size-y input))
    (map-grid ht input 0 0)

    (apply +
           (for/list ([y (range ymax)])
             (apply +
                    (for/list ([x (range xmax)])
                      (count-x-mas-at ht x y))))))

(check-equal? (part#2 test_input) 9)
(part#2 input)
