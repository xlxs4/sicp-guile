(import (chicken random))
(import (chicken time))

(define (square x) (* x x))

(define (fast-expt b n)
  (cond ((= n 0)
         1)
        ((even? n)
         (square (fast-expt b (/ n 2))))
        (else
         (* b (fast-expt b (- n 1))))))

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

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
  (fast-prime? n 20)) ; I lowered it to 20 for speed

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

(search-for-primes 1000 1061)
(search-for-primes 10000 10093)
(search-for-primes 100000 100151)

