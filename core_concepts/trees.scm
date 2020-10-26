#lang racket

(define (leftTree tree)
  (cadr tree))
(define (rightTree tree)
  (caddr tree))
(define (entry tree)
  (car tree))

(define (make-tree root l r)
  (list root l r))

(define (leaf? tree)
  (and (null? (rightTree tree))
       (null? (leftTree tree))))

(define (tree? t)
  (or (null? t)
      (and (list? t)
           (= (length t) 3)
           (tree? (leftTree t))
           (tree? (rightTree t)))))


;Sample tree here
(define t (make-tree 1 (leaf 2) (make-tree 3 (leaf 4) (leaf 5))))

(define (inorder tree)
  (cond ((null? tree) '())
        ((leaf? tree) (display (entry tree)) (display " "))
        (else (inorder (leftTree tree))
              (display (entry tree))
              (display " ")
              (inorder (rightTree tree)))))

;Sums the values in all vertices
(define (sumVert tree)
  (cond ((null? tree) 0)
        ((leaf? tree) (entry tree))
        (else (+ (entry tree)
           (sumVert (leftTree tree))
           (sumVert (rightTree tree))))))
(sumVert '(1 (-17 () ()) (2 () ())))

(define (min3 a b c)
  (min (min a b) c))


(define (minTree tree)
  (cond ((leaf? tree) (entry tree))
        ((null? (rightTree tree))
         (min (entry tree) (minTree (rightTree tree))))
        ((null? (leftTree tree))
         (min (entry tree) (minTree (leftTree tree))))
        (else (min3 (entry tree)
                    (minTree (leftTree tree))
                    (minTree (rightTree tree))))))

(define (depth tree)
  (if (null? tree) -1
      (+ 1 (max (depth (rightTree))
                (depth (leftTree))))))

;Verifies that the right side returnes successfully
(define (consCheck h t) (and t (cons h t)))

(define (mem-tree x tree)
  (cond ((null? tree) #f)
        ((eqv? x (entry tree)) #t)
        (else (or (mem-tree x (leftTree tree))
                  (mem-tree x (rightTree tree))))))


(define (path-tree x t)
  (cond ((empty-tree t) #f)
        ((eqv? x (entry t)) (list (entry t)))
        (else (consCheck (entry t)
                         (or (path-tree x (leftTree t))
                             (path-tree x (rightTree t)))))))
(define (1+tree tree)
  (if (null? tree) '()
      (make-tree (+ 1 (entry tree))
                 (1+tree (leftTree tree))
                 (1+tree (rightTree tree)))))

(define (map-tree f tree)
  (if (null? tree) '()
      (make-tree (f (entry tree))
                 (map-tree f (leftTree f tree))
                 (map-tree f (rightTree f tree)))))

(define (filter p l)
  (cond ((null? l) '())
        ((p (car l)) (cons (car l) (filter p (cdr l))))
        (else (filter p (cdr l)))))

(define (search p l)
  (if (null? l) #f
      (or (p (car l))
          (search p (cdr l)))))
(define (exists? p l)
  (not (null? (filter p l))))