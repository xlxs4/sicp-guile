;;; Exercise 2.41
;; Write a procedure to find all ordered triples
;; of distinct positive integers i, j, and k
;; less than or equal to a given integer n
;; that sum to a given integer s.

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

(define (sum xs)
  (accumulate + 0 xs))

;; You can see this technique of nested mappings
;; generalizes in an easy manner.
(define (unique-triples n)
  (flatmap
   (lambda (i)
     (flatmap (lambda (j)
            (map (lambda (k)
                   (list i j k))
                 (enumerate-interval 1 (- j 1))))
          (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

;; > (unique-triples 5)
;; '((3 2 1)
;;   (4 2 1)
;;   (4 3 1)
;;   (4 3 2)
;;   (5 2 1)
;;   (5 3 1)
;;   (5 3 2)
;;   (5 4 1)
;;   (5 4 2)
;;   (5 4 3))

(define (sum-triples-to-s s n)
  (filter
   (lambda (triple)
     (= (sum triple) s))
   (unique-triples n)))

;; > (sum-triples-to-s 10 5)
;; '((5 3 2) (5 4 1))
