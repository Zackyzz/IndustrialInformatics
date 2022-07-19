#lang racket

(define (knapsack n w)
  (define items (build-list n (λ _ (list (random 1 w) (random 1 (* 2 w))))))
  (printf "~a\n" items)
  (let loop ([B (list (build-list (+ 1 w) (λ _ 0)))] [clone items])
    (cond
      [(equal? empty clone) B]
      [else
       (define wk (caar clone))
       (define tail (last B))
       (define new (for/list ([j (+ 1 w)] [old tail])
                     (if (<= wk j)
                         (max old (+ (cadar clone) (list-ref tail (- j wk))))
                         old)))
       (loop (append B (list new)) (cdr clone))])))

(knapsack (read) 5)