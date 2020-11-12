
(define (1+ n) (+ 1 n))
(define (acc op nv a b term next)
  (if (> a b) nv
      (op (term a) (acc op nv (next a ) b term next))))
(define (pow x n)
  (acc * 1 1 n (lambda (i) x) 1+))


(define (binary-to-decimal number)
	(if (= number 0)
		0
		(+ (* 2 (binary-to-decimal (quotient number 10))) (remainder number 10))))
(define (binary-to-decimal-iter number)
	(define (helper num power accum)
		(if (= num 0)
			accum
			(helper (quotient num 10) (+ power 1) (+ accum (* (remainder num 10) (pow 2 power))))))
	(helper number 0 0))

(define (decimal-to-binary number)
	(if (< number 1)
		0
		(+ (remainder number 2) (* 10 (decimal-to-binary (quotient number 2))))))
(define (rev number)
	(define (helper num accum)
		(if (= num 0)
			accum
			(helper (quotient num 10) (+ (remainder num 10) (* accum 10)))))
	(helper number 0))

(define (set-add set elem)
  (define (iter rest i result)
    (cond ((and (> i elem) (= rest 0)) result)
          ((= i elem) (iter (quotient rest 10) (+ i 1) (+ result (pow 2 i))))
          (else (iter (quotient rest 10) (+ i 1) (+ result (* (remainder rest 10) (pow 2 i)))))))
  (iter (decimal-to-binary set) 0 0))

(define (set-remove set elem)
  (define (iter rest i result)
    (cond ((= rest 0) result)
          ((= i elem) (iter (quotient rest 10) (+ i 1) result))
          (else (iter (quotient rest 10) (+ i 1) (+ result (* (remainder rest 10) (pow 2 i)))))))
  (iter set 0 0))

(define (set-contains? set elem)
  (define (iter rest i)
    (cond ((or (= rest 0) (> i elem) (> 0 i)) #f)
          ((= i elem) (= (remainder rest 10) 1))
          (else (iter (quotient rest 10) (+ i 1)))))
  (iter set 0))

(define (set-empty? set)
  (= set 0))

(define (set-size set)
  (define (iter rest i count)
    (cond ((= rest 0) count)
          ((= (remainder rest 10) 1) (iter (quotient rest 10) (+ i 1) (+ count 1)))
          (else (iter (quotient rest 10) (+ i 1) count))))
  (iter set 0 0))

(define (set-intersect s1 s2)
  (define (iter r1 r2 i result)
    (cond ((or (= r1 0) (= r2 0))
           result)
          ((and (= (remainder r1 10) 1) (= (remainder r2 10) 1))
           (iter (quotient r1 10) (quotient r2 10) (+ i 1) (+ result (pow 2 i))))
          (else (iter (quotient r1 10) (quotient r2 10) (+ i 1) result))))
  (iter s1 s2 0 0))

(define (set-union s1 s2)
  (define (iter r1 r2 i result)
    (cond ((and (= r1 0) (= r2 0))
           result)
          ((or (= (remainder r1 10) 1) (= (remainder r2 10) 1))
           (iter (quotient r1 10) (quotient r2 10) (+ i 1) (+ result (* 1 (pow 2 i)))))
          (else (iter (quotient r1 10) (quotient r2 10) (+ i 1) result))))
  (iter s1 s2 0 0))

(define (set-difference s1 s2)
  (define (iter r1 r2 i result)
    (cond ((= r1 0)
           result)
          ((= (remainder r1 10) (remainder r2 10))
           (iter (quotient r1 10) (quotient r2 10) (+ i 1) result))
          ((= (remainder r1 10) 1)
           (iter (quotient r1 10) (quotient r2 10) (+ i 1) (+ result (pow 2 i)) ))
          (else (iter (quotient r1 10) (quotient r2 10) (+ i 1) result))))
  (iter s1 s2 0 0))

;Section included for testing-----------


;Info can be modified to test knapsack solution
(define initial-capacity 190)
(define w-list '(56
59
80
64
75
17
))
(define p-list '(50
50
64
46
50
 5))

(define (elem-i i lst)
  (if (= i 0) (car lst)
      (elem-i (- i 1) (cdr lst))))
(define (w i)
  (elem-i i w-list))
(define (p i)
  (elem-i i p-list))


;Gives a list which shows the elements included in the set
(define (give-list set)
  (define (iter res i lst)
    (cond ((= res 0) lst)
          ((= (remainder res 10) 1)
           (iter (quotient res 10) (+ i 1) (cons i lst)))
          (else (iter (quotient res 10) (+ i 1) lst))))
  (iter (decimal-to-binary set) 0 '()))

(define (list->dec-set lst)
  (define (iter xs my-set)
    (if (null? xs) my-set
        (iter (cdr xs) (set-add  my-set (car xs)) )))
  (iter lst 0))

;---------------------End of test section


(define (give-set-value set)
  (define (iter res i sum)
    (cond ((= res 0) sum)
          ((= (remainder res 10) 1)
           (iter (quotient res 10) (+ i 1) (+ (p i) sum)))
          (else (iter (quotient res 10) (+ i 1) sum))))
  (iter (decimal-to-binary set) 0 0))

;Usual recursive solution of knapsack problem. Gives value of maximum profit.
(define (knapsack-rec c n w p)
  (cond ((or (= n 0) (= c 0)) 0)
        ((> (w (- n 1)) c)
         (knapsack-rec c (- n 1) w p))
        (else 
         (max (+ (p (- n 1)) (knapsack-rec (- c (w (- n 1))) (- n 1) w p) )
              (knapsack-rec c (- n 1) w p)))))

;Gives a set with maximum profit.
(define (knapsack c n w p)
  (define (build-set current-set space-left i)
    (cond ((or (> i n) (= space-left 0)) current-set)
          ((> (w i) space-left)
           (build-set current-set space-left (+ i 1)))
          ((> (give-set-value
               (build-set current-set space-left (+ i 1)))
              (give-set-value
               (build-set (set-add current-set i) (- space-left (w i)) (+ i 1))) )
           (build-set current-set space-left (+ i 1)))
          (else
           (build-set (set-add current-set i) (- space-left (w i)) (+ i 1)) )))
  (build-set 0 c 0))

(define solution-set (knapsack initial-capacity (- (length p-list) 1) w p))

;Show the solution in three forms
;(knapsack-rec initial-capacity (- (length p-list) 1) w p)
;(decimal-to-binary solution-set)
;(give-list solution-set)

