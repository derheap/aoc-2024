#lang racket
(require rackunit)
(require racket/trace)

(define input (file->string "input/11.txt"))
(define test_input "125 17")
(define (split-line line)
  (string-split line #px"\\s+"))


(define (even-digits? str)
  (even? (string-length str)))

(check-equal? (even-digits? "1") #f)
(check-equal? (even-digits? "21") #t)

(define (trim-stone stone)
  (let ([trimmed
         (string-trim stone "0" #:right? #f #:repeat? #t)])
    (if (string=? trimmed "") "0" trimmed)))

(define (split stone)
  (let* ([len (string-length stone)]
         [half (/ len 2)])
    (cons (trim-stone (substring stone 0 half))
          (trim-stone (substring stone half len)))))

(check-equal? (split "12") '("1" . "2"))
(check-equal? (split "1234") '("12" . "34"))

(define (is-null? stone)
  (string=? stone "0"))

(define (mult stone)
  (number->string (* (string->number stone) 2024)))

(check-equal? (mult "1") "2024")
(check-equal? (mult "2") "4048")

(define (add-stone ht stone cnt)
  (hash-set! ht stone (+ (hash-ref ht stone 0) cnt)))
(define (add-stones ht pair cnt)
  (hash-set! ht (car pair) (+ (hash-ref ht (car pair) 0) cnt))
  (hash-set! ht
             (cdr pair)
             (+ (hash-ref ht (cdr pair) 0) cnt)))

(define (dump ht)
  (hash-for-each
   ht
   (λ (k v)
     (if (> v 0)
         (println
          (string-append (number->string v) " x " k))
         null)))
  ht)

(define (blink ht)
  ; Handle single stone
  (define (blink-stone stone cnt)
    (cond
      ; only handle active stones. Stupid me.
      [(> cnt 0)
       (hash-set! ht stone (- (hash-ref ht stone) cnt))

       (cond
         [(is-null? stone) (add-stone ht "1" cnt)]
         [(even-digits? stone)
          (add-stones ht (split stone) cnt)]
         [else (add-stone ht (mult stone) cnt)])

       ht]
      [else ht]))

  ; Handle all stones
  (map (λ (pair) (blink-stone (car pair) (cdr pair)))
       (hash->list ht))

  ht)


(define (blink-often ht idx max)
  (cond
    [(< idx max) (blink-often (blink ht) (+ idx 1) max)]
    [else ht]))



(define (count-stones ht)
  (foldl (λ (value sum) (+ value sum)) 0 (hash-values ht)))

(define (part#1 input)
  (define ht (make-hash))
  (for-each (λ (num) (hash-set! ht num 1))
            (split-line input))
  (count-stones (blink-often ht 0 25)))

;(check-equal? (part#1 test_input) 22)
(part#1 input)

(define (part#2 input)
  (define ht (make-hash))
  (for-each (λ (num) (hash-set! ht num 1))
            (split-line input))
  (count-stones (blink-often ht 0 75)))


;(check-equal? (part#2 test_input) 31)
(part#2 input)