#lang racket
(require rackunit)
(require racket/trace)


(define input (file->string "input/13.txt"))
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

(define (parse-block str delta)
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
         (+ (string->number (sixth parts)) delta)
         (+ (string->number (seventh parts))delta)))


(define (calc-x p a b)
  (+ (* a (prize-ax p)) (* b (prize-bx p))))
(define (calc-y p a b)
  (+ (* a (prize-ay p)) (* b (prize-by p))))
(check-equal? (calc-x (prize 94 34 22 67 8400 5400) 80 40)
              8400)
(check-equal? (calc-y (prize 94 34 22 67 8400 5400) 80 40)
              5400)

(define (slopes p)
  (let ([ps (/(prize-y p)  (prize-x p))]
        [as (/(prize-ay p)  (prize-ax p))]
        [bs (/(prize-by p)  (prize-bx p))])
    (println (list ps as bs))))

(define (reaches-prize? p a b)
  (and (= (prize-x p) (calc-x p a b))
       (= (prize-y p) (calc-y p a b))))

(define (calc-a-from-b p b)
  (/ (- (prize-x p) (* b (prize-bx p))) (prize-ax p)))

(define (calc-for-b p b)
  (define a (calc-a-from-b p b))
  (list a b (reaches-prize? p a b)))

(define (iter-b p b max-b)
  ;(println (list max-b b))
  (define a (calc-a-from-b p b))
  (cond
    [(reaches-prize? p a b) (println (list a b))(+ (* a 3) b)]
    [(> b max-b) 0]
    [else (iter-b p (+ b 1) max-b)]))

(define (iter-b-reverse p b max-b)
  ;(println (list max-b b))
  (define a (calc-a-from-b p b))
  (cond
    [(reaches-prize? p a b) (println (list a b))(+ (* a 3) b)]
    [(< b (/ max-b 2)) 0]
    [else (iter-b-reverse p (- b 1) max-b)]))


(define (part#1 input)
  (define machines
    (map (λ (block) (parse-block block 0))
         (split-blocks input)))
  (define solutions (map (λ (machine) (iter-b machine 0 100)) machines))
  (foldl (λ (value sum) (+ value sum)) 0 solutions)
  )

(check-equal? (part#1 test_input) 480)
(println (part#1 input))

; Way to slow
(define offset 10000000000000)
(define (part#2 input)
  (define machines
    (map (λ (block) (parse-block block offset))
         (split-blocks input)))
  (for-each (λ (m) (println (list (prize-x m) (prize-y m)))) machines)
  (define m (first machines))
  (define bmax (floor(/ (prize-x m) (prize-bx m))))
  (println (list (prize-x m) (prize-bx m) bmax (* bmax (prize-bx m)) (* bmax (prize-by m)) ))
  (println (slopes m))
  ;(define solutions (map (λ (machine) (iter-b machine 0 (floor (/ (prize-x machine) (prize-bx machine))))) machines))
  ;(println solutions)
  ;(foldl (λ (value sum) (+ value sum)) 0 solutions)
  )


;(check-equal? (part#2 test_input) 31)
(part#2 test_input)