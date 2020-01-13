#lang racket
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
; 
(define lat?
  (lambda (l)
    (cond ((null? l) #t)
          ((atom? l) #f) ; bug p 16 little schemer , need to handle l being an atom
          ((atom? (car l)) (lat? (cdr l)))
          (else #f))))
(define member?
  (lambda (a l)
    (cond ((null? l) #f)
          (else (or (eq? a (car l)) (member? a (cdr l)))))))       

