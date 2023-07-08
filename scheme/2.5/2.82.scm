;;; Exercise 2.82
;; Show how to generalize apply-generic to handle coercion
;; in the general case of multiple arguments. One strategy is
;; to attempt to coerce all the arguments to the type of the first
;; argument, then to the type of the second argument, and so on.
;; Give an example of a situation where this strategy (and likewise
;; the two-argument version given above) is not sufficiently general.
;; (Hint: Consider the case where there are some suitable mixed-type
;; operations present in the table that will not be tried.)

(define (apply-generic op . args)
  (define (no-method-err op type-tags)
    ;; Error function when no method is found.
    (error "No method for these types"
           (list op type-tags)))

  (define (find-generic-type args type-tags)
    ;; Find a type to which all arguments can be coerced.
    (cond ((null? type-tags) #f)
          ((null? args) (car type-tags))
          (else
           (find-generic-type (cdr args)
                              (find-coercion-types (car args)
                                                   type-tags)))))

  (define (find-coercion-types arg type-tags)
    ;; Get a list of types to which the argument can be coerced.
    (filter (lambda (t2)
              (or (equal? (type-tag arg) t2)
                  (get-coercion (type-tag arg) t2)))
            type-tags))

  (define (coerce-all target-type)
    ;; Coerce all arguments to target type.
    (map (lambda (arg)
           (let ((arg-type (type-tag arg)))
             ((get-coercion arg-type target-type) arg)))
         (filter (lambda (arg) (not (equal? (type-tag arg) target-type)))
                 args)))

  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags))
         (target-type (find-generic-type args type-tags)))
    ;; Apply the procedure if it exists.
    ;; If it doesn't, try to find a common type and coerce all args to that type.
    ;; If there's no common type, error out.
    (cond (proc (apply proc (map contents args)))
          (target-type (apply apply-generic
                       (cons op (coerce-all target-type))))
          (else (no-method-err op type-tags)))))
