;;;; SPDX-FileCopyrightText: 2020 Vladimir Nikishkin <lockywolf@gmail.com>
;;;; SPDX-FileCopyrightText: 2021 Vasilij Schneidermann <mail@vasilij.de>
;;;;
;;;; SPDX-License-Identifier: MIT

;;; Constants

(define true #t)
(define false #f)
(define nil '())

;;; Random numbers.

(define (random x) ;; (chicken random)
  (if (exact-integer? x)
      (pseudo-random-integer x)
      (* x (pseudo-random-real))))

;;; Timing.

#>
#include <sys/time.h>
static struct timeval t0;
static int t0_initialized = 0;
<#

(define runtime ;; (chicken foreign)
  (foreign-lambda* double ()
    "struct timeval t1;"
    "if (!t0_initialized) {"
    "  gettimeofday(&t0, NULL);"
    "  t0_initialized = 1;"
    "}"
    "gettimeofday(&t1, NULL);"
    "C_return((t1.tv_sec - t0.tv_sec) * 1000 + (t1.tv_usec - t0.tv_usec) / 1000.0);"))

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
