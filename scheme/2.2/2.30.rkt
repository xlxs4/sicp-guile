;;; Exercise 2.30
;; Define a procedure square-tree analogous to
;; the square-list procedure of Exercise 2.21.
;; That is, square-tree should behave as follows:

;; (square-tree
;;  (list 1
;;        (list 2 (list 3 4) 5)
;;        (list 6 7)))
;; (1 (4 (9 16) 25) (36 49))

(define (square-tree tree)
  (let ((square (lambda (x) (* x x))))
    (cond ((null? tree) '())
          ((not (pair? tree))
           (square tree))
          (else
           (cons (square-tree (car tree))
                 (square-tree (cdr tree)))))))

(define (square-tree tree)
  (let ((square (lambda (x) (* x x))))
    (map (lambda (sub-tree)
           (if (pair? sub-tree)
               (square-tree sub-tree)
               (square sub-tree)))
         tree)))
