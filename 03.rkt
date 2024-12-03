#lang racket
(require rackunit)
(require racket/match)
(require racket/trace)

(define input (file->string "input/03.txt"))

(define test_input
  "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(define (mul-ok? inst)
  (regexp-match? #rx"^mul\\(([0-9]+),([0-9]+)\\)" inst))

(define (inst-ok? inst)
  (regexp-match?
   #rx"^mul\\(([0-9]+),([0-9]+)\\)|don't\\(\\)|do\\(\\)"
   inst))

(define (run-mul inst)
  (match (regexp-match #rx"^mul\\(([0-9]+),([0-9]+)\\)"
                       inst)
    [(list _ a b)
     (* (string->number a) (string->number b))]))

(define (part#1 input)
  (foldr (lambda (curr accu) (+ curr accu))
         0
         (map run-mul
              (filter mul-ok?
                      (regexp-split #rx"(?=mul)" input)))))

(check-equal? (part#1 test_input) 161)
(part#1 input)

(define (split-input input)
  (regexp-split #rx"(?=mul)|(?=don't\\(\\))|(?=do\\(\\))"
                input))

(define (run list do sum)
  (cond
    [(empty? list) sum]
    [(string=? (substring (first list) 0 4) "do()")
     (run (rest list) #t sum)]
    [(string=? (substring (first list) 0 4) "don'")
     (run (rest list) #f sum)]
    [(eq? do #f) (run (rest list) do sum)]
    [else
     (run (rest list) do (+ (run-mul (first list)) sum))]))
;(trace run)
;(trace run-mul)

(define (part#2 input)
  (run (filter inst-ok? (split-input input)) #t 0))

(check-equal? (part#2 test_input) 48)
(part#2 input)