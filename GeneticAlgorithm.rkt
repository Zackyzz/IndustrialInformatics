#lang racket

;(define N 30)
(define DIMENSION 2)

(define (file-lines->list path)
  (call-with-input-file path
    (λ (file)
      (for/list ([line (in-lines file)])
        (map string->number (string-split line))))))

(define towns (file-lines->list "towns.txt"))
;(define towns (build-list N (λ _ (for/list ([i (in-range DIMENSION)]) (random 1 255)))))

(define (generate-population chromosome n)
  (cons chromosome (build-list (- n 1) (λ _ (shuffle chromosome)))))

(define (distance p q)
  (sqrt (for/fold ([sum 0]) ([i (in-range DIMENSION)])
          (+ sum (sqr (- (list-ref p i) (list-ref q i)))))))

(define (tour-distance lst)
  (let loop ([sum (distance (first lst) (last lst))] [clone lst])
    (cond
      [(= 1 (length clone)) sum]
      [else (loop (+ sum (distance (first clone) (second clone))) (rest clone))])))

(define (sort-by-fitness lists)
  (sort lists > #:key (λ(x) (/ 1 (tour-distance x)))))

(define (mutate towns i j)
  (define clone (list->vector (map list->vector towns)))
  (define temp (vector-ref clone j))
  (vector-set! clone j (vector-ref clone i))
  (vector-set! clone i temp)
  (vector->list (vector-map vector->list clone)))

(define (clone-population tours)
  (define the-elite (take (sort-by-fitness tours) (quotient (length tours) 2)))
  (define mutated-tours (for/list ([tour (in-list the-elite)])
                          (mutate tour (random 0 (length tour)) (random 0 (length tour)))))
  (append the-elite mutated-tours))

(define (go-ga towns)
  (define initial-population (generate-population towns 10))
  (define initial-distance (tour-distance (first initial-population)))
  (printf "Initial town: ~a\nInitial distance: ~a\n"
          (first initial-population) initial-distance)
  (let loop ([clone initial-population] [distances (list initial-distance)] [t 1])
    (cond
      [(= t 500) (list distances clone)]
      [else
       (define bestest (tour-distance (first clone)))
       (printf "\nEpoch ~a: Tour distance: ~a" t bestest)
       (loop (clone-population clone) (append distances (list bestest)) (+ 1 t))])))

(define best-tour (time (go-ga towns)))
(last (first best-tour))
(second (first best-tour))

(define (distances->file lst path)
  (let loop ([clone lst] [out (open-output-file path #:exists 'replace)])
    (if (empty? clone)
        (close-output-port out)
        (begin
          (display (first clone) out)
          (display " " out)
          (unless (empty? (rest clone)) (display "\n" out))
          (loop (rest clone) out)))))

(distances->file (first best-tour) "gaDistances.txt")