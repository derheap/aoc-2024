#lang racket
(require rackunit)

(define input (string-trim (file->string "input/09.txt")))
(define test_input "2333133121414131402")

(define (parse data)
  (map (λ (s) (string->number s))
       (filter (λ (s) (not (equal? s "")))
               (string-split data ""))))

(define (expand-entry entry is-file? id)
  (build-list entry
              (lambda (_) (if is-file? id #\.))))

(define (expand result data is-file? id)
  (define hd (first data))
  (define tl (rest data))
  (if (empty? tl)
      (reverse (cons (expand-entry hd is-file? id)
                     result))
      (expand (cons (expand-entry hd is-file? id)
                    result)
              tl
              (not is-file?)
              (if is-file?
                  (+ id 1)
                  id))))

(define (compact vtoc blocks)
    (_compact '()  (first vtoc) (rest vtoc) (first blocks) (rest blocks)))

(define (_compact result hd-vtoc vtoc hd-blocks blocks)
  ;(println result)
  ;(println hd-vtoc)
  ;(println vtoc)
  ;(println hd-blocks)
  ;(println blocks)
  ;(println "-----")
  (cond
      [(empty? (rest vtoc)) (reverse  result)]
      ; Use free space
      [(equal? hd-vtoc #\.) (_compact (cons hd-blocks result) (first vtoc) (rest (drop-right vtoc 1)) (first blocks) (rest blocks))]
      [else (_compact (cons hd-vtoc result) (first vtoc) (rest vtoc) hd-blocks blocks)]
      )
)

(define (check-sum vtoc idx sum)
    (cond
        [(empty? vtoc) sum]
        [else (check-sum (rest vtoc) (+ idx 1) (+ sum (* idx (first vtoc))))])
    )

(define (part#1 input)
  (define vtoc
    (list->vector (flatten (expand null (parse input) #t 0))))
  (define len (vector-length vtoc))
  ;(define blocks (filter (lambda (c) (not (equal? c #\.)))(reverse vtoc)))
  (println len)
  ;(check-sum (compact vtoc blocks) 0 0)
  )

(check-equal? (part#1 test_input) 1928)
(part#1 input)

(define (part#2 input)
  null)

;(check-equal? (part#2 test_input) 31)
;(part#2 test_input)
