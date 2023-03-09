;;; Exercise 2.31
;; Abstract your answer to Exercise 2.30
;; to produce a procedure tree-map with the property
;; that square-tree could be defined as

;; (define (square-tree tree)
;;   (tree-map square tree))

(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map f sub-tree)
             (f sub-tree)))
       tree))
