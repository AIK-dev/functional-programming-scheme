#lang racket

(define (sncoc x l) (append l (list x)))
(define (atom? l) (and (not (pair? l)) (not (null? l))))

(define (foldr op nv l)
  (if (null? l) nv
      (op (car l) (foldr op nv (cdr l)))))

(define (deep-foldr nv term op dl)
  (foldr op nv (map
                (lambda (x)
                  (if (atom? x)
                      (term x)
                      (deep-foldr nv term op x))) dl)))

(define (countAtoms l)
  (deep-foldr 0 (lambda (x) 1) + l))

(define (flatten l)
  (deep-foldr '() list append l))

;Need to identify the difference between this
(define (deepRev l)
  (cond ((null? l) l)
        ((atom? l) l)
        (else (append (deepRev (cdr l)) (list (deepRev (car l)))))))
;... and this.
(define (drBottom l)
  (cond ((null? l) l)
        ((atom? l) (list l)) ;This is a incorrect bottom case anyway. This accepts l which is not list
        (else (append (drBottom (cdr l)) (drBottom (car l))))))

(define dl '((1 (2)) (((3) 4) (5 (6)) () (7)) 8))
(drBottom '(2))

