;;; Exercise 2.40
;; Define a procedure unique-pairs that, given
;; an integer n, generates the sequence of pairs
;; (i,j) with i <= j < i <= n. Use unique-pairs
;; to simplify the definition of prime-sum-pairs
;; given above.

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor))))) ; <next> used here

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low
            (enumerate-interval
             (+ low 1)
             high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair)
        (cadr pair)
        (+ (car pair (cadr pair)))))

;; Just extracting from prime-sum-pairs as-is.
(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j)
            (list i j))
          (enumerate-interval
           1
           (- i 1))))
   (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter
        (prime-sum?
         (unique-pairs n)))))
