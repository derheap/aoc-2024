#lang racket
(require rackunit)

(define input (file->lines "input/01.txt"))
(define test_input
  (string-split "3   4
4   3
2   5
1   3
3   9
3   3" "\n"))
(define (split-line line) (string-split line #px"\\s+"))
(define (split-lines line) (map split-line line))
(define (first-list pair) (string->number (first pair)))
(check-equal? (first-list '("1" "2")) 1)

(define (second-list pair) (string->number (second pair)))
(check-equal? (second-list '("1" "2")) 2)

(define (left input)
    (sort (map first-list (split-lines input)) <))
(check-equal? (left test_input) '(1 2 3 3 3 4))

(define (right input)
    (sort (map second-list (split-lines input)) <))
(check-equal? (right test_input) '(3 3 3 4 5 9))

(define (part#1 input)
  (apply +
         (map (lambda (a b) (abs (- a b)))
              (left input)
              (right input))))

(check-equal? (part#1 test_input) 11)
(part#1 input)

(define (count-in-list my-num list)
  (length (filter (lambda (num) (= num my-num)) list)))
(check-equal? (count-in-list 3 '(3 3 3 4 5 9)) 3)
(check-equal? (count-in-list 9 '(3 3 3 4 5 9)) 1)

(define (part#2 input)
  (apply +
         (map (lambda (num)
                (* num (count-in-list num (right input))))
              (left input))))

(check-equal? (part#2 test_input) 31)
(part#2 input)
