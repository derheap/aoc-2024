#lang racket
(require rackunit)
(require racket/trace)

(define input (file->lines "input/02.txt"))
(define test_input
  (string-split "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9" "\n"))
(define (split-line line)
  (string-split line #px"\\s+"))
(define (split-line->ints line)
  (map string->number
    (string-split line #px"\\s+")))
(define (split-lines line)
  (map split-line line))
(define (split->ints line)
  (map split-line->ints line))

(define (direction delta)
  (cond
  [(< delta 0) -1]
  [(> delta 0) 1]
  [else 0])
)
(check-equal? (direction 3) 1)
(check-equal? (direction -2) -1)
(check-equal? (direction 0) 0)

(define (deltas list)
  (define (iter result list last)
    (cond
    [(empty? (rest list)) (cons (-  (first list)  last) result)]
    [else (iter (cons (- (first list) last)  result) (rest list) (first list))]
  )
  )
  (iter '() (rest list) (first list))
)
(check-equal? (deltas '(7 6 4 2 1)) '(-1 -2 -2 -1))

(define (safe-asc? list)
  (andmap (lambda (it) (and (>= it 1) (<= it 3))) list)
)

(define (safe-desc? list)
  (andmap (lambda (it) (and (<= it -1) (>= it -3))) list)
)
(check-equal? (safe-desc? '(-1 -2 -2 -1)) #t)

(define (is-safe? list)
  (cond
  [(< (first list) 0) (safe-desc? list)]
  [(> (first list) 0) (safe-asc? list)]
  [ else #f]
  )
)

(check-equal? (is-safe? (deltas '(7 6 4 2 1))) #t)
(check-equal? (is-safe? (deltas '(1 2 7 8 9))) #f)

(define (part#1 input)
  (length (filter (lambda (it) (eq? #t it))
    (map (lambda (line)
      (is-safe? (deltas line)))(split->ints input)) )))

(check-equal? (part#1 test_input) 2)
(part#1 test_input)

(define (is-safe-damped? list)
  {cond
    [ (is-safe? (deltas list)) #t]
    [ else  (ormap (lambda (combination)(is-safe? (deltas combination)))
                   (combinations list (- (length list) 1)))
    ]
 }
)
;(trace is-safe-damped?)
;(trace deltas)
(define (part#2 input)
  (length
   (filter
    (lambda (it) (eq? #t it))
    (map (lambda (line)
           (is-safe-damped?  line))(split->ints input)) )))


(check-equal? (part#2 test_input) 4)
(part#2 input)
