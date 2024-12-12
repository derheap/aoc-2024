#lang racket
(require rackunit)
(require racket/vector)

(define input (string-trim (file->string "input/09.txt")))
(define test_input "2333133121414131402")

(define (parse data)
  (map (λ (s) (string->number s))
       (filter (λ (s) (not (equal? s "")))
               (string-split data ""))))

(define (expand-entry entry is-file? id)
  (build-list entry (lambda (_) (if is-file? id #\.))))

(define (expand result data is-file? id)
  (define hd (first data))
  (define tl (rest data))
  (if (empty? tl)
      (reverse (cons (expand-entry hd is-file? id) result))
      (expand (cons (expand-entry hd is-file? id) result)
              tl
              (not is-file?)
              (if is-file?
                  (+ id 1)
                  id))))

(define (check-sum vtoc idx sum max)
  (define id (vector-ref vtoc idx))
  (cond
    [(>= idx max) sum]
    [else
     (check-sum vtoc
                (+ idx 1)
                (+ sum (* idx (if (equal? id #\.) 0 id)))
                max)]))

(define (defragment vtoc idx file-idx len)
  ;(println (list  vtoc idx file-idx))
  (cond
    [(>= idx len) vtoc]
    [(>= idx file-idx) vtoc]
    [(equal? (vector-ref vtoc file-idx) #\.)
     (defragment vtoc idx (- file-idx 1) len)]
    [(equal? (vector-ref vtoc idx) #\.)
     (begin
       (vector-set! vtoc idx (vector-ref vtoc file-idx))
       (vector-set! vtoc file-idx #\.)
       (defragment vtoc (+ idx 1) (- file-idx 1) len))]
    [else (defragment vtoc (+ idx 1) file-idx len)]))

(define (part#1 input)
  (define vtoc
    (list->vector
     (flatten (expand null (parse input) #t 0))))
  (define len (vector-length vtoc))
  (check-sum (defragment vtoc 0 (- len 1) len)
             0
             0
             (- len 1)))

(check-equal? (part#1 test_input) 1928)
;(part#1 input)

; file descriptor
(struct fd (fid len start))
; free space
(struct free (len start))

(define (is-free? item)
  (equal? item #\.))

(define (is-free-ref? vtoc idx)
  (equal? (vector-ref vtoc idx) #\.))

(define (find-heighest-id vtoc tail-idx)
  (define fid (vector-ref vtoc tail-idx))
  (if (is-free? fid)
      (find-heighest-id vtoc (- tail-idx 1))
      (fd fid null tail-idx)))

(define (get-fd vtoc fid tail-idx found? len)
  ; the fid was found. idx point to next candidate.
  (define (scan-for-len idx len)
    (define id (vector-ref vtoc idx))
    (cond
      [(<= idx 0) (fd fid len (+ idx 1))]
      [(equal? fid id) (scan-for-len (- idx 1) (+ len 1))]
      [else (fd fid len (+ idx 1))]))
  ; find the fid, starting at idx.
  (define (scan-for-id idx)
    (define id (vector-ref vtoc idx))
    (cond
      [(<= idx 0) (fd fid len (+ idx 1))]
      [(equal? fid id) (get-fd vtoc fid (- idx 1) #t 1)]
      [else (scan-for-id (- idx 1))]))
  (if found?
      (scan-for-len tail-idx len)
      (scan-for-id tail-idx)))

(define (find-free-space vtoc len idx last-idx free-len)
  (cond
    [(>= free-len len) (fd #\. len (- idx len))]
    [(>= idx last-idx) #f]
    [(is-free-ref? vtoc idx)
     (find-free-space vtoc
                      len
                      (+ idx 1)
                      last-idx
                      (+ free-len 1))]
    [else (find-free-space vtoc len (+ idx 1) last-idx 0)]))

(define (set-vtoc vtoc pos len data)
  (cond
    [(< len 1) vtoc]
    [else
     (vector-set! vtoc pos data)
     (set-vtoc vtoc (+ pos 1) (- len 1) data)]))

(define (move vtoc file space)
  (set-vtoc vtoc (fd-start file) (fd-len file) #\.)
  (set-vtoc vtoc
            (fd-start space)
            (fd-len space)
            (fd-fid file))
  vtoc)

(define (defragment#2 vtoc last-idx file-id)
  (let* ([f (get-fd vtoc file-id last-idx #f 0)]
         [s (find-free-space vtoc (fd-len f) 0 last-idx 0)])
    ;(println (list "F:" (fd-fid f) (fd-start f) (fd-len f)))
    (cond
      ; found space
      [(< file-id 0) vtoc]
      [s
       ;(println
      ;(list "S:" (fd-fid s) (fd-start s) (fd-len s)))
       (move vtoc f s)
       ;(println vtoc)
       (defragment#2 vtoc (fd-start f) (- file-id 1))]
      [(>= file-id 0)
       (defragment#2 vtoc (fd-start f) (- file-id 1))]
      [else vtoc])))

(define (part#2 input)
  (let* ([vtoc (list->vector
                (flatten (expand null (parse input) #t 0)))]
         [last-idx (- (vector-length vtoc) 1)]
         [file-id (find-heighest-id vtoc last-idx)])

  (define new-vtoc (defragment#2 vtoc last-idx (fd-fid file-id)))
  ;(println new-vtoc)
   (check-sum new-vtoc
       0
       0
       (- last-idx 0))))

(check-equal? (part#2 test_input) 2858)
(part#2 input)
