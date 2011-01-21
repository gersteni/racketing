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

;; my super-shortend version using let

(define (insertR new old lat)
  (let ((rec (lambda ()(insertR new old (cdr lat)))))
  (cond 
    ((null? lat) '())
    ((eq? old (car lat))
     (cons old (cons new (rec))))
    (else (cons (car lat) (rec))))))

;; my shorter version, using inline define, even better!

(define (insertR new old lat)
  (define (rec)
    (insertR new old (cdr lat)))
  (cond 
    ((null? lat) '())
    ((eq? old (car lat))
     (cons old (cons new (rec))))
    (else (cons (car lat) (rec)))))


;; my version inserts a new one everywhere new appears, which
;; differs from the by the book method, which just does the sub
;; the first time

(define (insertL new old lat)
  (cond
    ((null? lat) '())
    ((eq? old (car lat)) (cons new (cons old (insertL new old (cdr lat)))))
    (else (cons (car lat) (insertL new old (cdr lat))))))

(define (subst new old lat)
  (cond
    ((null? lat) '())
    ((eq? old (car lat)) (cons new (cdr lat)))
    (else (cons (car lat) (subst new old (cdr lat))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) '())
      ((or (eq? (car lat) o1)
           (eq? (car lat) o2))
       (cons new (cdr lat)))
      (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))


;; trying out ormap. Doesn't make things all that smaller

(define (subst2 new o1 o2 lat)
  (cond 
    ((null? lat) '())
    ((ormap (lambda (x) 
              (eq? (car lat) x)) 
            (list o1 o2))
     (cons new (cdr lat)))
    (else (cons (car lat) (subst2 new o1 o2 (cdr lat))))))


(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? a (car lat)) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))

;; Chapter 5, TLS

;; below is a useful list for testing
(define tst 
  '(a b c (a b d) (a (b c))))

(define (rember* a l) 
  (cond 
    ((null? l) '()) 
    ((atom? (car l)) 
     (cond ((eq? (car l) a) (rember* a (cdr l))) 
           (else (cons (car l) (rember* a (cdr l))))))
    (else (cons (rember* a (car l)) (rember* a (cdr l))))))


;; the simple version
(define (insertR* new old l)
  (cond
    ((null? l) '())
    ((atom? (car l))
     (cond
       ((eq? (car l) old)
        (cons old (cons new (insertR* new old (cdr l)))))
       (else (cons (car l) (insertR* new old (cdr l))))))
    (else (cons (insertR* new old (car l)) (insertR* new old (cdr l))))))

;; trying to simplify it

(define (insertR* new old l)
  (define (rec)
    (insertR* new old (cdr l)))
  (cond
    ((null? l) '())
    ((atom? (car l))
     (cond
       ((eq? (car l) old)
        (cons old (cons new (rec))))
       (else (cons (car l) (rec)))))
    (else (cons (insertR* new old (car l)) (rec)))))

(define (occur* a l)
  (cond
    ((null? l) 0)
    ((atom? (car l))
     (cond
       ((eq? (car l) a) (+ 1 (occur* a (cdr l))))
       (else (occur* a (cdr l)))))
    (else (+ (occur* a (car l)) (occur* a (cdr l))))))

(define (subst* new old l)
  (cond
    ((null? l) '())
    ((atom? (car l))
     (cond
       ((eq? old (car l)) (cons new (subst* new old (cdr l))))
       (else (cons (car l) (subst* new old (cdr l))))))
    (else (cons (subst* new old (car l))
                (subst* new old (cdr l))))))

;; skipping insertL*, member* because they would be really easy

;; also checks for null leftmost, in difference with the book
(define (leftmost l)
  (cond
    ((null? l) '())
    ((atom? (car l)) (car l))
    (else (leftmost (car l)))))

(define (eqlist? a b)
  (cond
    ((and (null? a) (null? b)) #t)
    ((or (null? a) (null? b)) #f)
    ((and (atom? (car a)) 
          (atom? (car b)))
     (and (eq? (car a) (car b))
          (eqlist? (cdr a) (cdr b))))
    (else (and (eqlist? (car a) (car b)))
          (eqlist? (cdr a) (cdr b)))))


;; skipping chapter 6, b/c it is boring looking (doing manual math sucks)

(define (set? lat)
  (cond
    ((null? lat) #t)
    ((member? (car lat) (cdr lat)) #f)
    (else (set? (cdr lat)))))

;; something's up with the book - makes the right set, wrong order
(define (makeset lat)
  (cond
    ((null? lat) '())
    ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
    (else (cons (car lat) (makeset (cdr lat))))))

(define (makeset lat)
  (cond
    ((null? lat) '())
    (else (cons (car lat)
                (makeset (multirember (car lat) (cdr lat)))))))

(define (subset? a b)
  (cond
    ((null? a) #t)
    ((member? (car a) b)
     (subset? (cdr a) b))
    (else #f)))

(define (subset? a b)
  (cond
    ((null? a) #t)
    (else (and (member? (car a) b)
               (subset? (cdr a) b)))))

(define (eqset? a b)
  (and (subset? a b)
       (subset? b a)))

(define (intersect? a b)
  (cond
    ((null? a) #f) ;needed to support null sets
    (else (or (member? (car a) b)
              (intersect? (cdr a) b)))))

(define (intersect a b)
  (cond
    ((null? a) '())
    ((member? (car a) b)
     (cons (car a) (intersect (cdr a) b)))
    (else (intersect (cdr a) b))))


