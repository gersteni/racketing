;; These are notes from The Little Schemer


;; Preface - to get things right

(define atom?
  (lambda (x)
    (and (not (pair? x)) 
         (not (null? x)))))


;; Chapter 1 of The Little Schemer - nothing here

;; Chapter 2

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

;; my first cut, without using or
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((eq? a (car lat)) #t)
      (else (member? a (cdr lat))))))

;; the book's version, using or

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? a (car lat))
                (member? a (cdr lat)))))))

;; chapter 3


;; my version of rember (and the book's, too!)

(define (rember a lat)
  (cond
    ((null? lat) '())
    ((eq? a (car lat)) (rember a (cdr lat)))
    (else (cons (car lat) 
                (rember a (cdr lat))))))


;; my version of firsts

(define (firsts l)
  (cond
    ((null? l) '())
    (else (cons (caar l) (firsts (cdr l))))))


;; my version of insertR
;; more like insertR*


(define (insertR new old lat)
  (cond 
    ((null? lat) '())
    ((eq? old (car lat))
     (cons old (cons new (insertR new old (cdr lat)))))
    (else (cons (car lat) (insertR new old (cdr lat))))))

;; my super-shortend version

(define (insertR new old lat)
  (let ((rec (lambda ()(insertR new old (cdr lat)))))
  (cond 
    ((null? lat) '())
    ((eq? old (car lat))
     (cons old (cons new (rec))))
    (else (cons (car lat) (rec))))))
