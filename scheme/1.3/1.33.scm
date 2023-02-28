;;; Exercise 1.33
;; You can obtain an even more general version
;; of accumulate (Exercise 1.32) by introducing
;; the notion of a filter on the terms to be
;; combined. That is, combine only those terms
;; derived from values in the range that satisfy
;; a specified condition. The resulting filtered-accumulate
;; abstraction takes the same arguments as accumulate,
;; together with an additional predicate of one argument
;; that specifies the filter. Write filtered-accumulate
;; as a procedure. Show how to express the following
;; using filtered-accumulate:

;; 1. the sum of the squares of the prime numbers in the
;; interval a to b (assuming that you have a prime?
;; predicate already written)

;; 2. the product of all the positive integers less than n
;; that are relatively prime to n (i.e., all positive
;; integers i < n such that GCD(i, n) ⇒ 1.)

(define (inc x) (+ x 1))
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

;; Recursive version
(define (filtered-accumulate
         combiner null-value term a next b filter?)
  (if (> a b)
      null-value
      (combiner
       (if (filter? a)
           (term a)
           null-value)
       (filtered-accumulate
        combiner null-value term (next a) next b filter?))))

;; Iterative version
(define (filtered-accumulate
         combiner null-value term a next b filter?)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner
                        result
                        (if (filter? a)
                            (term a)
                            null-value)))))
  (iter a null-value))

(define (sum-prime-squares a b)
  (filtered-accumulate + 0 square a inc b prime?))

(define (prod-rel-primes n)
  (define (relative-prime? i)
    (= (gcd i n) 1))
  (filtered-accumulate * 1 identity 1 inc n relative-prime?))
