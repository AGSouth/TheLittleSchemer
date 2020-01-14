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


(define rember
  (lambda (a lat)
    (cond ((null? lat) (quote ()))
          (else (cond
                  ((eq? a (car lat)) (cdr lat))
                    (else (cons (car lat)
                                (rember a (cdr lat))))
                  )))))

(define rember1
  (lambda (a lat)
    (cond ((null? lat) (quote ()))
          ((eq? (car lat) a) (cdr lat))                   
          (else (cond
                  ((eq? a (car lat)) (cdr lat))
                    (else (cons (car lat)
                                (rember1 a (cdr lat))))
                  )))))

(define firsts
  (lambda (l)
  (cond ((null? l) (quote ()))
        (else
          (cons (car (car l)) (firsts (cdr l)))))))