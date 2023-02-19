(import (chicken random))
(import (chicken time))

(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (pseudo-random-integer (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n)
         (fast-prime? n (- times 1)))
        (else #f)))

(define (prime? n)
  (fast-prime? n 10000)) ; Crank that up!

(define (timed-prime-test n)
  (start-prime-test n (current-milliseconds)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (current-milliseconds)
                         start-time))))

(define (report-prime n elapsed-time)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (newline))

(define (search-for-primes start end)
  (define (iter n)
    (cond ((<= n end)
           (timed-prime-test n) (iter (+ n 2)))
          (else
           (newline))))
  (if (even? start)
      (iter (+ 1 start))))

(search-for-primes 10000000000 10000000207)
(search-for-primes 100000000000 100000000151)
(search-for-primes 1000000000000 1000000000193)
(search-for-primes 10000000000000 10000000000293)

