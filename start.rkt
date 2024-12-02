#lang racket
(require rackunit)

;(define input (file->lines "input/02.txt"))
(define test_input
  (string-split "3   4
4   3
2   5
1   3
3   9
3   3" "\n"))
(define (split-line line) (string-split line #px"\\s+"))
(define (split-lines line) (map split-line line))


(define (part#1 input)
  'null )

(check-equal? (part#1 test_input) 11)
(part#1 test_input)

(define (part#2 input)
 'null)

(check-equal? (part#2 test_input) 31)
(part#2 test_input)
