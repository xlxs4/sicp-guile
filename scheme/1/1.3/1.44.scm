;;; Exercise 1.44
;; The idea of *smoothing* a function is an important
;; concept in signal processing. If f is a function
;; and dx is some small number, then the smoothed
;; version of f is the function whose value at point
;; x is the average of f(x - dx), f(x), and f(x + dx).
;; Write a procedure smooth that takeas as input a
;; procedure that computes f and returns a procedure
;; that computes the smoothed f. It is sometimes valuable
;; to repeatedly smooth a function (that is, smooth the
;; the smoothed function, and so on) to obtain the n-fold
;; smoothed function. Show how to generate the n-fold
;; smoothed function of any given function using smooth
;; and repeated from Exercise 1.43.

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (define (iter n acc)
    (cond ((= 1 n) acc)
          ((even? n) (double (iter (/ n 2) acc)))
          (else (compose f (iter (- n 1) (compose f acc))))))
  (iter n f))

(define (smooth f)
  (define dx 0.00001)
  (lambda (x)
    (/
     (+ (f (- x dx))
        (f x)
        (f (+ x dx)))
     3)))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))
