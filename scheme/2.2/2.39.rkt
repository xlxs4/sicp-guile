;;; Exercise 2.39
;; Complete the following definitions of reverse
;; (Exercise 2.18) in terms of fold-right and
;; fold-left from Exercise 2.38:

;; (define (reverse sequence)
;;   (fold-right
;;    (lambda (x y) <??>) nil sequence))

;; (define (reverse sequence)
;;   (fold-left
;;    (lambda (x y) <??>) nil sequence))

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;; cons gets us close, but the nesting is messed up.
;; Remember we can use append instead.
;; However, we need to (list x) to have something to
;; append y to, since append needs to be applied to a
;; list.

(define (reverse sequence)
  (fold-right
   (lambda (x y) (append y (list x))) '() sequence))

;; Nothing to do here.
(define (reverse sequence)
  (fold-left
   (lambda (x y) (cons y x)) '() sequence))
