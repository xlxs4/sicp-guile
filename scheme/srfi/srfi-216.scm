;;; -*- mode: scheme; -*-
;; Time-stamp: <2021-01-06 22:42:17 lockywolf>
;; Title: srfi-216 sample implementation.
;; Author: lockywolf
;; Created: <2020-11-03 Tue>

;;; r4rs booleans

(define true #t)
(define false #f) ;; luckily, SICP does not use '() as false

;;; Empty list
(define nil '())

;;; Random numbers.

(define (random x) ;; srfi-27
  (if (exact-integer? x)
     (random-integer x)
     (* x (random-real))))

;;; Timing.

(define (runtime) ;; r7rs
  (round (* (current-jiffy) (jiffies-per-second) #e1e6))) ;; microseconds

;;; Multi-threading.

(define (parallel-execute . forms) ;; srfi-18
  (let ((myo (open-output-string)))
    (define (create-threads . forms)
      (if (null? forms)
	  (list)
	  (let ((ctxi (thread-start!
		       (make-thread
			(lambda () (parameterize ((current-output-port myo))
				((car forms))))))))
	    (cons ctxi (apply create-threads (cdr forms))))))
    (define (wait-threads thread-list)
      (if (null? thread-list)
	  #t
	  (begin (thread-join! (car thread-list))
		 (wait-threads (cdr thread-list)))))
    (wait-threads (apply create-threads forms))
    (display (get-output-string myo)))) ;; return value is not specified by SICP

(define central-old-mutex (make-mutex 'global-srfi-18-mutex)) ;; not exported

(define (test-and-set! cell) ;; srfi-18
  (mutex-lock! central-old-mutex)
  (let ((output (if (car cell) #t (begin (set-car! cell #t) #f))))
    (mutex-unlock! central-old-mutex)
    output))

;;; Streams.

(define-syntax cons-stream ;; r7rs
  (syntax-rules ()
    ((cons-stream a b) (cons a (delay b)))))

(define stream-null? null?)

(define the-empty-stream '())

