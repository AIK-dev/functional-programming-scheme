#lang racket

(define (tabulate f start end step_count)
  (let ((step (/ (- end start) step_count)) )

    (display step)
  (define (iter lst a b)
    (if (> a b) lst
        (iter (cons (list a (f a)) lst)
              (+ a step) b)))
    (iter '() start end)))

(define (func x)
  (/ (* (abs (- x 2)) (abs (- x 2)))
     (+ (* x x) 1)))

(tabulate func 17 234.55 30)

  
