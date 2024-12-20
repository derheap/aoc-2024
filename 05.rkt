#lang racket
(require rackunit)

;(define input (file->lines "input/02.txt"))
(define test_input
  "47|53
  97|13
  97|61
  97|47
  75|29
  61|13
  75|53
  29|13
  97|29
  53|29
  61|53
  97|53
  61|29
  47|13
  75|47
  97|75
  47|61
  75|61
  47|29
  75|13
  53|13

  75,47,61,53,29
  97,61,53,29,13
  75,29,13
  75,97,47,61,53
  61,13,29
  97,13,75,29,47")
(define (split-line line)
  (string-split line #px"\\s+"))
(define (split-lines line)
  (map split-line line))
(define (split-parts text)
  (string-split text "\n\n"))



(define (parse-rules text)
  (map (λ (line)
         (map string->number
              (string-split (string-trim line) "|")))
       (string-split text "\n")))
(define (parse-pages text)
  (map (λ (line)
         (map string->number
              (string-split (string-trim line) ",")))
       (string-split text "\n")))

(define (page-ok? suc-rules pre-rules pre page rest)
  page)

(define (print-run-ok? suc-rules pre-rules pre page rest)
  (println (list pre page rest))
  page)

(define (part#1 input)
  (define suc (make-hash))
  (define pre (make-hash))
  (let* ([rules (parse-rules (first (split-parts input)))]
         [pages (parse-pages (second (split-parts input)))])
    (map (λ (pair)
           (hash-set! suc (first pair) (second pair)))
         rules)
    (map (λ (pair)
           (hash-set! pre (second pair) (first pair)))
         rules)
    (map
     (λ (page-run)
       (print-run-ok? suc pre '() (first page-run) (rest page-run)))
       pages)))
;(check-equal? (part#1 test_input) 143)
(part#1 test_input)

;(define (part#2 input) 'null)

;(check-equal? (part#2 test_input) 31)
;(part#2 test_input)