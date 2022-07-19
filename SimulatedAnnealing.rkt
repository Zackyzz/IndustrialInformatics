#lang racket

(define N 30)
(define DIMENSION 2)

(define (file-lines->list path)
  (call-with-input-file path
    (λ (file)
      (for/list ([line (in-lines file)])
        (map string->number (string-split line))))))

(define towns (file-lines->list "towns.txt"))
;(define towns (build-list N (λ _ (for/list ([i (in-range DIMENSION)]) (random 1 255)))))

(define (distance p q)
  (sqrt (for/fold ([sum 0]) ([i (in-range DIMENSION)])
          (+ sum (sqr (- (list-ref p i) (list-ref q i)))))))

(define (swap! towns i j)
  (define clone (list->vector (map list->vector towns)))
  (define temp (vector-ref clone j))
  (vector-set! clone j (vector-ref clone i))
  (vector-set! clone i temp)
  (vector->list (vector-map vector->list clone)))

(define (full-dist lst)
  (let loop ([sum (distance (first lst) (last lst))] [clone lst])
    (cond
      [(= 1 (length clone)) sum]
      [else (loop (+ sum (distance (first clone) (second clone))) (rest clone))])))

(define (SA lst)
  (define decay 0.96)
  (let loop ([dist (full-dist lst)] [T 1000] [clone lst] [t 1] [distances (list (full-dist lst))])
    (printf "Epoch ~a: ~a\n" t dist)
    (define new-towns (swap! clone (random 0 (length clone)) (random 0 (length clone))))
    (define new-dist (full-dist new-towns))
    (cond
      [(< T 1) (list dist distances)]
      [(< new-dist dist)
       (loop new-dist (* T decay) new-towns (+ 1 t) (append distances (list new-dist)))]
      [else
       (define P (exp (- (/ (- new-dist dist) T))))
       (if (> P (random))
           (loop new-dist (* T decay) new-towns (+ 1 t) (append distances (list new-dist)))
           (loop dist (* T decay) clone (+ 1 t) (append distances (list dist))))])))

(define solution (SA towns))
(first solution)

(define (list->file lst path)
  (let loop ([clone lst] [out (open-output-file path #:exists 'replace)])
    (if (empty? clone)
        (close-output-port out)
        (begin
          (for ([i (in-range (length (first clone)))])
            (display (list-ref (first clone) i) out)
            (when (< (+ i 1) (length (first clone))) (display " " out)))
          (unless (empty? (rest clone)) (display "\n" out))
          (loop (rest clone) out)))))

(define (list->file2 lst path)
  (let loop ([clone lst] [out (open-output-file path #:exists 'replace)])
    (if (empty? clone)
        (close-output-port out)
        (begin
            (display (first clone) out)
            (display " " out)
          (unless (empty? (rest clone)) (display "\n" out))
          (loop (rest clone) out)))))

(list->file2 (second solution) "distances.txt")
