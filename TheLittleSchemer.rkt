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
; note b must be positive whole numbers
(define +
  (lambda (a b)
    (cond ((zero? b) a)
           (else (+ (add1 a) (sub1 b))))))

(define - 
  (lambda (a b)
    (cond ((zero? b) a)
           (else (- (sub1 a) (sub1 b))))))

(define addtup
  (lambda (tup)
    (cond ((null? tup) 0)
           (else (+ (car tup) (addtup (cdr tup)))))))

(define x
  (lambda (m n)
    (cond ((zero?  n) 0)
           (else (+ m (x  m (sub1 n)))))))


(define tupeq+
  (lambda (tup1 tup2)
    (cond ((and (null? tup1) (null? tup2)) '())
           (else (cons  (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))


(define tup+
  (lambda (tup1 tup2)
    (cond ((null? tup1) tup2)
          ((null? tup2) tup1)
           (else (cons  (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(define >
   (lambda (m n)
   ;   (cond ((and (zero? m) (zero? n)) false)
     (cond ((zero? m) false)
     ((zero? n) true)
     (else (> (sub1 m) (sub1 n))))))


(define <
   (lambda (m n)
  ;  (cond ((and (zero? m) (zero? n)) false)
     (cond ((zero? n) false)
           ((zero? m) true)
           (else (< (sub1 m) (sub1 n))))))
;
;(define =
; (lambda (m n)
;       (cond ((and (zero? m) (zero?  n)) true)
;             ((or (zero? m) (zero? n)) false)
;             (else (= (sub1 m) (sub1 n))))))
; from the book, elegant
(define =
  (lambda (m n)
    (cond ((zero? m) (zero? n))
          ((zero? n) false)
          (else (= (sub1 m) (sub1 n))))))

;
(define ^
  (lambda (m n)
    (cond ((zero? n) 1)
          (else (x m (^ m (sub1 n)))))))

(define ???
  (lambda (m n)
    (cond ((< m n) 0)
          (else (add1 (??? (- m n) n))))))


(define division
  (lambda (m n)
    (cond ((< m n) 0)
          (else (add1 (??? (- m n) n))))))

(define length
    (lambda (lat)
      (cond ((null? lat) 0)
            (else (add1 (length (cdr lat)))))))

(define pick
     (lambda (n lat)
         (cond 
               ((null? lat) 'badlist)
               ((= n 0) 'badn)
               ((= n 1) (car lat))
               (else (pick (sub1 n) (cdr lat))))))

(define rempick
     (lambda (n lat)
         (cond 
               ((null? lat) '())
               ((= n 1)  (cdr lat))
               (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))


(define nonums
     (lambda (lat)
         (cond ((null? lat) '())
               ((number? (car lat)) (nonums (cdr lat)))
               (else (cons (car lat) (nonums (cdr lat)))))))


(define all-nums
     (lambda (lat)
         (cond ((null? lat) '())
               ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
               (else (all-nums (cdr lat))))))

(define eqan
     (lambda (a1 a2)
       (cond ((and (number? a1) (number? a2)) (= a1 a2))
             ((or (number? a1) (number? a2)) false)
             (else (eq? a1 a2)))))

(define occur
   (lambda (a lat)
      (cond ((null? lat) 0)
            ((eqan a (car lat)) (+ 1 (occur a (cdr lat))))
            (else (occur a (cdr lat))))))


(define one?
   (lambda (n)
     (= 0 (sub1 n))))

; Chapter 5 begins

(define rember*
  (lambda (a l)
    (cond ((null? l) '())
          ((atom? (car l))
                    (cond ((eq? a (car l)) (rember* a (cdr l)))
                          (else (cons (car l) (rember* a (cdr l))))))
          (else (cons (rember* a (car l)) (rember* a (cdr l)))))))

(define lat
  (lambda (l)
    (cond ((null? l) true)
          ((atom? (car l)) (lat (cdr l)))
          (else false))))

(define insertR*
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((atom? (car lat))
                 (cond ((eq? old (car lat)) (cons (car lat) (cons new (insertR* new old (cdr lat)))))
                    (else (cons (car lat) (insertR* new old (cdr lat))))))
            (else (cons (insertR* new old (car lat)) (insertR* new old (cdr lat)))))))            