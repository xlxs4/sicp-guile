;;; Exercise 2.27
;; Modify your reverse procedure of Exercise 2.18
;; to procduce a deep-reverse procedure that takes
;; a list as argument and returns as its value the
;; list with its elements reversed an with all
;; sublists deep-reversed as well. For example,

;; (define x
;;   (list (list 1 2) (list 3 4)))

;; x
;; ((1 2) (3 4))

;; (reverse x)
;; ((3 4) (1 2))

;; (deep-reverse x)
;; ((4 3) (2 1))

(define (deep-reverse xs)
  (define (reverse xs)
    (define (iter l1 l2)
      (if (null? l1)
          l2
          (iter (cdr l1) (cons (car l1) l2))))
    (iter xs '()))
  (if (pair? xs)
      (reverse (map deep-reverse xs))
      xs))
