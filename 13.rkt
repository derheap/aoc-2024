#lang racket
(require rackunit)
(require racket/trace)

(define input (file->string "input/11.txt"))
(define test_input
  "Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279")

(define (split-blocks line)
  (string-split line #px"\n\n"))

(struct prize (ax ay bx by x y))

(define (parse-block str)
  (define parts
    (regexp-match
     #rx"Button A: X([+-][0-9]+), Y([+-][0-9]+)
Button B: X([+-][0-9]+), Y([+-][0-9]+)
Prize: X=([0-9]+), Y=([0-9]+)"
     str))
  (prize (string->number (second parts))
         (string->number (third parts))
         (string->number (fourth parts))
         (string->number (fifth parts))
         (string->number (sixth parts))
         (string->number (seventh parts))))

(define (reaches-prize? p a b)
  (and (= (prize-x p)
           (+ (* a (prize-ax p) (* b (prize-bx p)))))
       (= (prize-y p)
           (+ (* a (prize-ay p) (* b (prize-by p)))))))

(define (calc-a-from-b p b)
  (/ (- (prize-x p) (* b (prize-bx p)))
     (prize-ax p)
     ))

(define (calc-for-b p b)
  (define a (calc-a-from-b p b))
(list a b (reaches-prize? p a b))
  )

(define (part#1 input)
  (define machines 
  (map (λ (block) (parse-block block))
       (split-blocks input)))
   (calc-for-b (first machines) 40))

;(check-equal? (part#1 test_input) 480)
(part#1 test_input)

(define (part#2 input)
  null)


;(check-equal? (part#2 test_input) 31)
;(part#2 input)