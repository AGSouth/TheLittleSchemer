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

; Chapter 3 The Littls Schemer

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

(define insertR
  (lambda (new old l)
    (cond ((null? l) '())
          ((eq? (car l) old) (cons (car l) (cons new (cdr l))))
          (else (cons (car l) (insertR new old (cdr l)))))))


(define insertL
  (lambda (new old l)
    (cond ((null? l) '())
          ((eq? (car l) old) (cons new l))
          (else (cons (car l) (insertL new old (cdr l)))))))

(define subst 
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? (car lat) old) (cons new (cdr lat)))
          (else (cons (car lat) (subst new old (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond ((null? lat) (quote()))
          ((eq? (car lat) a) (multirember a (cdr lat)))                 
          (else (cons (car lat) (multirember a (cdr lat)))))))


(define multiinsertR
  (lambda (new old l)
    (cond ((null? l) '())
          ((eq? (car l) old) (cons (car l) (cons new (multiinsertR new old (cdr l)))))
          (else (cons (car l) (multiinsertR new old (cdr l)))))))


(define multisubst 
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? (car lat) old) (cons new (multisubst  new old (cdr lat))))
          (else (cons (car lat) (multisubst new old (cdr lat)))))))


(define multiinsertL
  (lambda (new old l)
    (cond ((null? l) '())
          ((eq? (car l) old) (cons new (cons (car l) (multiinsertL new old (cdr l)))))
          (else (cons (car l) (multiinsertL new old (cdr l)))))))

; Chapter 4 The Little Schemer