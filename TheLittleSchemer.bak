#lang racket
(require racket/trace)
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

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

(define rember2
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

(define occur*
  (lambda (a l)
    (cond ((null? l) 0)
          ((atom? (car l))
             (cond ((eq? a (car l)) (add1 (occur* a (cdr l))))
                   (else (occur* a (cdr l)))))
          (else (+ (occur* a (car l)) (occur* a (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond ((null? l) '())
          ((atom? (car l))
                 (cond ((eq? old (car l)) (cons new (subst* new old (cdr l))))
                       (else (cons (car l) (subst* new old (cdr l))))))
          (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

 (define insertL*
   (lambda (new old l)
     (cond ((null? l) '())
            ((atom? (car l))
                      (cond ((eq? old (car l)) (cons new (cons (car l) (insertL* new old (cdr l)))))
                            (else (cons (car l) (insertL* new old (cdr l))))))
            (else (cons (insertL* new old (car l))  (insertL* new old (cdr l)))))))
(trace insertL*)

(define member*
  (lambda (a l)
   (cond ((null? l) false)
           ((atom? (car l))
            (cond ((eq? a (car l)) true)
                  (else (member* a (cdr l)))))
          (else (or (member? a (car l)) (member* a (cdr l)))))))

(define leftmost
  (lambda (l)
     (cond ((null? l) false)
           ((cond ((atom? (car l)) (car l))
            (else (or (leftmost  (car l)) (leftmost (cdr l)))))))))

(trace leftmost)

(define eqlist
  (lambda (l1 l2)
    (cond ((and (null? l1) (null? l2)) true)
          ((or (null? l1) (null? l2)) false)
          ((and (atom? (car l1)) (atom? (car l2)))
               (cond ((or (eqan (car l1) (car l2)) (eq? (car l1) (car l2)))
                        (eqlist (cdr l1) (cdr l2)))
                      (else false)))
          ((or (atom? (car l1)) (atom? (car l2)) false))
          (else (and (eqlist (car l1) (car l2)) (eqlist (cdr l1) (cdr l2)))))))

(define equal?
   (lambda (s1 s2)
     (cond ((and (null? s1) (null? s2)) true)
          ((or (null? s1) (null? s2)) false)
          ((and (atom? s1) (atom? s2))
             (cond ((eq? s1 s2) true)
                   (else false)))
          ((or (atom? s1) (atom? s2)) false)
             (else (and (equal? (car s1) (car s2)) (equal? (cdr s1) (cdr s2)))))))


; final rember from chapter 5
(define rember
   (lambda (s l) ; note    any s - expression, not just an atom
     (cond
        ((null? l) '())
        ((equal? (car l) s ) (cdr l))
        (else (cons (car l) (rember s (cdr l)))))))

;; end of Chapter 5

;; Chapter 6
;; note: this is wrong since it returns true for (1 1) which i snot a aexp, so ... need to fix...
(define numberedv1?
  (lambda (x)
      (cond ((null? x) true)
            ((atom? x) (or (number? x) (eq? x '+) (eq? x 'x) (eq? x '^) (eq? x '/)))
            (else (and (numbered? (car x)) (numbered? (cdr x)))))))

(define numberedv2?
  (lambda (aexp)
      (cond ((null? aexp) true)
            ((atom? aexp) (number? aexp))
            ((and (number? (car aexp)) (or (eq? (car(cdr aexp)) '+) (eq? (car (cdr aexp)) 'x) (eq? (car( cdr aexp))  '^)
                                           (eq? (car (cdr aexp)) '/))) (numbered? (cdr (cdr aexp))))
            (else false))))


(define numbered-bad? ; page 101 inexplicably bad code in The Little Schemer, endless loop for '(1 + 2) !!
  (lambda (aexp)
    (cond ((atom? aexp) (number? aexp))
          (else
           (and (numbered? aexp)
                (numbered?
                   (car (cdr (cdr aexp)))))))))

(define numbered?  ; bug in this version too, (numbered? '(1 1) returns void instead of false?? fixed by else statement
   (lambda (aexp)
     (cond
         ((atom? aexp) (number? aexp))
         ((eq? (car (cdr aexp)) (quote +))
             (and (numbered? (car aexp))
                  (numbered? (car (cdr (cdr aexp))))))
         ((eq? (car (cdr aexp)) (quote -))
             (and (numbered? (car aexp))
                  (numbered? (car (cdr (cdr aexp))))))
        ((eq? (car (cdr aexp)) (quote /))
             (and (numbered? (car aexp))
                  (numbered? (car (cdr (cdr aexp))))))
         ((eq? (car (cdr aexp)) (quote x))
             (and (numbered? (car aexp))
                  (numbered? (car (cdr (cdr aexp))))))
         ((eq? (car (cdr aexp)) (quote ^))
             (and (numbered? (car aexp))
                  (numbered? (car (cdr (cdr aexp))))))
         (else false))))

(trace numbered?)

;(define value=v1?  ; this is really buggy code (1 + 2 + 3) return 3 ... need to fix at some time
;   (lambda (aexp)
;     (cond
;         ((atom? aexp) aexp)
;          ;(cond ((number? aexp) aexp)
          ;        (else false)))
;         ((eq? (car (cdr aexp)) (quote +))
         ;    (cond ((and (value? (car aexp))
         ;         (value? (car (cdr (cdr aexp))))))
 ;                  (+ (car aexp) (value-v1? (car (cdr (cdr aexp))))))
     ;     ((eq? (car (cdr aexp)) (quote -))
     ;        (and (numbered? (car aexp))
     ;             (numbered? (car (cdr (cdr aexp))))))
     ;   ((eq? (car (cdr aexp)) (quote /))
     ;        (and (numbered? (car aexp))
     ;             (numbered? (car (cdr (cdr aexp))))))
     ;    ((eq? (car (cdr aexp)) (quote x))
     ;        (and (numbered? (car aexp))
     ;             (numbered? (car (cdr (cdr aexp))))))
     ;    ((eq? (car (cdr aexp)) (quote ^))
     ;        (and (numbered? (car aexp))
     ;             (numbered? (car (cdr (cdr aexp))))))
  ;       (else false))))
         

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
   (lambda (aexp)
     (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))
         
(define value-chapter5
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator nexp) (quote +))
       (+ (value (1st-sub-exp nexp))
          (value (2nd-sub-exp nexp)))))))



                 
 ; end of Chapter 6


 ; start of chapter 7
(define set?
   (lambda (l)
     (cond ((null? l) true)
           ((member? (car l) (cdr l)) false)
           (else (set? (cdr l))))))

(define makeset
 (lambda (lat)
   (cond ((null? lat) '())
         ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
          (else (cons (car lat) (makeset (cdr lat)))))))


(define makesetv2
 (lambda (lat)
   (cond ((null? lat) '())
         (else (cons (car lat)  (makesetv2 (multirember (car lat) lat)))))))

(define subset?
  (lambda (s1 s2)
    (cond ((null? s1) true)
          ((member? (car s1) s2) (subset? (cdr s1) s2))
          (else false))))


(define subset-v2?
  (lambda (s1 s2)
    (cond ((null? s1) true)
          (else 
          (and (member? (car s1) s2) (subset-v2? (cdr s1) s2))))))

(define eqset?
  (lambda (s1 s2)
    (and (subset? s1 s2) (subset? s2 s1))))

(define intersect?
   (lambda (s1 s2)
     (cond ((null? s1) false)
           (else
            (or (member? (car s1) s2) (intersect? (cdr s1) s2))))))

(define intersect
   (lambda (s1 s2)
     (cond ((null? s1)  '())
           ((member? (car s1) s2) (cons (car s1) (intersect (cdr s1) s2)))
           (else (intersect (cdr s1) s2)))))

(define union
    (lambda (s1 s2)
      (cond ((null? s1) s2)
           ((member? (car s1) s2) (union (cdr s1) s2))
           (else (cons (car s1) (union (cdr s1) s2))))))

(define intersect-all
   (lambda (l-set)
     (cond ((null? (cdr l-set)) '())
           (else (union (intersect (car l-set) (car (cdr l-set))) (intersect-all (cons (car l-set) (cdr (cdr l-set)))))))))

; the book version is much more elegant than my version ... 
(define intersect-all-book
    (lambda (l-set)
      (cond ((null?  (cdr l-set)) (car l-set))
            (else (intersect (car l-set)
                      (intersect-all (cdr l-set)))))))

(define a-pair?
  (lambda (x)
    (cond ((null? x) false)
          ((atom? x) false)
          ((null? (cdr x)) false)
          ((null? (cdr (cdr x))) true)
          (else false))))

; not sure why authorse used this form

(define first-book
  (lambda (a)
         (cond
           (else (car a)))))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define third
  (lambda (p)
    (cdr (cdr p))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define fun?
   (lambda (rel)
     (set? (firsts rel))))

(define revrel-v1
  (lambda (rel)
    (cond ((null? rel) '())
          (else (cons (build (second (car rel)) (first (car rel))) (revrel (cdr rel)))))))

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))


(define revrel
  (lambda (rel)
    (cond ((null? rel) '())
          (else (cons (revpair (car rel)) (revrel (cdr rel)))))))

                 

(define seconds
  (lambda (l)
  (cond ((null? l) (quote ()))
        (else
          (cons (second (car l)) (seconds (cdr l)))))))

(define fullfun?
   (lambda (fun)
     (set? (seconds fun))))

; end of chapter 7

; start Chapter 8

(define rember-f-v1
   (lambda (test? s l) ; note    any s - expression, not just an atom
     (cond
        ((null? l) '())
        ((test? (car l) s ) (cdr l))
        (else (cons (car l) (rember-f test? s (cdr l)))))))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

 (define eq?-salad (eq?-c 'salad))

(define rember-f
   (lambda (test?)
   (lambda (s l) ; 
     (cond
        ((null? l) '())
        ((test? (car l) s ) (cdr l))
        (else (cons (car l) ((rember-f test?) s (cdr l))))))))

(define rember-eq? (rember-f eq?))


(define insertL-f
  (lambda (test?)
  (lambda (new old l)
    (cond ((null? l) '())
          ((test? (car l) old) (cons new l))
          (else (cons (car l) ((insertL-f  test?) new old (cdr l))))))))

(define insertL-eq (insertL-f eq?))

(define insert-g-JM
    (lambda (dir?)
      (lambda (new old l)
      (cond ((eq? dir? 'R) (insertR new old l))
            (else (insertL new old l)))))) ; yep Know this is wrong, should test explicitly for L and err ...

(define insert-Right-JM (insert-g-JM 'R))


(define insert-Left-JM (insert-g-JM 'L))

 ; ok the above 3 functions handle using existing insertR / insertL books wants us to get
 ; experience passing in code

(define seqL
       (lambda (new old l)
         (cons new (cons old l)))) ; 

(define seqR
       (lambda (new old l)
         (cons old  (cons new ))))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((eq? old (car l))
          (seq new old (cdr l)))
        (else  (cons (car l) ((insert-g seq) new old (cdr l))))))))

(define insertR1 (insert-g seqR))

(define insertL1 (insert-g seqL))

(define seqS
  (lambda (new old l)
    (cons new l)))

(define subst1 (insert-g seqS))

(define atom-to-function
  (lambda (y)
       (cond ((eq? y '+) +)
             ((eq? y 'x) x) ; when x was the parameter, this did not work! beause x = 'x ... neat bug
             ((eq? y '^) ^)
             ((eq? y '^)))))

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (((atom-to-function (operator nexp))
          (value (1st-sub-exp nexp))
          (value (2nd-sub-exp nexp)))))))

(define multirember-f
         (lambda (test?)
           (lambda (a lat)
             (cond ((null? lat) '())
                   ((test? a (car lat))
                     ((multirember-f test?) a (cdr lat)))
                   (else
                     (cons (car lat)  ((multirember-f test?) a (cdr lat))))))))

(define multirember-eq? (multirember-f eq?))

(define eq-tuna?
  (eq?-c 'tuna))

(define multiremberT
  (lambda (test? lat)
     (cond ((null? lat) '())
           ((test? (car lat))
                (multiremberT test? (cdr lat)))
           (else
               (cons (car lat) (multiremberT test? (cdr lat)))))))


(define multirember&co
  (lambda (a lat col)
    (cond ((null? lat)
             (col '() '()))
          ((eq? (car lat) a)
             (multirember&co a (cdr lat)
                          (lambda (newlat seen)
                            (col newlat (cons (car lat) seen)))))
          (else
             (multirember&co a
                             (cdr lat)
                             (lambda (newlat seen)
                               (col (cons (car lat) newlat) seen)))))))
                                
(define a-friend
  (lambda (x y)
    (null? y)))

(define new-friend
  (lambda (newlat seen)
    (a-friend newlat (cons (car lat) seen))))

 (define latest-friend
   (lambda (newlat seen)
     (a-friend (cons 'and newlat) seen)))

 (define last-friend
   (lambda (x y)
     (length x)))

(define multiinsertLR
   (lambda (new oldL oldR lat)
     (cond
       ((null? lat) (quote ()))
       ((eq? (car lat) oldL)
         (cons new
               (cons oldL
                     (multiinsertLR new oldL oldR (cdr lat)))))
       ((eq? (car lat) oldR)
         (cons oldR
               (cons new
                     (multiinsertLR new oldL oldR (cdr lat)))))
       (else (cons (car lat)
                     (multiinsertLR new oldL oldR (cdr lat)))))))

(define multiinsertLR&co
         (lambda (new oldL oldR lat col)
           (cond
             ((null? lat)
               (col '() 0 0))
             ((eq? (car lat) oldL)
              (multiinsertLR&co new oldL oldR (cdr lat)
                   (lambda (newlat L R)
                     (col (cons new
                             (cons oldL newlat))
                             (add1 L) R))))
             ((eq? (car lat) oldR)
              (multiinsertLR&co new oldL oldR (cdr lat)
                   (lambda (newlat L R)
                     (col (cons oldR 
                             (cons new newlat))
                              L (add1 R)))))
             (else
                (multiinsertLR&co new oldL oldR (cdr lat)
                     (lambda (newlat L R)
                       (col (cons (car lat) newlat)
                            L R)))))))

(define mycol-list
  (lambda (newlist L R)
     newlist))

(define mycol-counts
  (lambda (newlist L R)
     (cons L (cons R '()))))


(define myeven? ; note this does not work ... for future functions using libary function even? 
  (lambda (n)
    (= (* 2 (/ n 2)) n)))

(define evens-only*
  (lambda (l)
    (cond ((null? l) '())
          ((atom? (car l))
            (cond 
             ((even? (car l)) (cons (car l) (evens-only* (cdr l))))
             (else (evens-only* (cdr l)))))
          
          (else
           (cons (evens-only* (car l)) (evens-only* (cdr l)))))))

; (trace evens-only)

 (define evens-only*&co
   (lambda (l col)
     (cond ((null? l) (col '() 1 0))
           ((atom? (car l))
              (cond ((even? (car l))
                      (evens-only*&co (cdr l)
                       (lambda (newl p s)            
                          (col (cons (car l) newl) (x (car l) p) s))))
                    (else (evens-only*&co (cdr l)
                           (lambda (newl p s)
                             (col newl p (+ (car l) s)))))))
           (else
             (evens-only*&co (car l)
                               ;    (evens-only*&co (cdr l)
                                        (lambda (al ap as)
                                          (evens-only*&co (cdr l)
                                               (lambda (dl dp ds)
                                                 (col (cons al dl)
                                                 (x ap dp)
                                                 (+ as ds))))))))))

; the above is buggy need to revisit

(define the-last-friend
  (lambda (newl product sum)
    (cons sum (cons product newl))))
