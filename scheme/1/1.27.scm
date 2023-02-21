;;; Exercise 1.27
;; Demonstrate that the Carmichael numbers listed in
;; Footnote 47 really do fool the Fermat test. That is,
;; write a procedure that takes an integer n and tests
;; whether a^n is congruent to a modulo n for every a < n,
;; and try your procedure on the given Carmichael numbers.

(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n)
         n)
        ((divides? test-divisor n)
         test-divisor)
        (else (find-divisor
               n
               (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (carmichael? n)
  (define (try-it n a)
    (cond ((or (= n 0) (= n 1)) #f)
          ((= a n) #t)
          ((not (= (expmod a n n) (remainder a n))) #f)
          (else (try-it n (+ a 1)))))
  (try-it n 1))

(map carmichael? '(561 1105 1729 2465 2821 6601)) ; Carmichael numbers
(map prime? '(561 1105 1729 2465 2821 6601)) ; Not primes!
