;; an atom, an S-expression that is not a list
(define atom?
  (lambda (x)
    (and (not (pair? x))
         (not (null? x)))))

;; wether the list is made of ONLY atoms
(define lat?
  (lambda (l)
    (cond
     [(null? l) #t]
     [(atom? (car l)) (lat? (cdr l))]
     [else #f])))

;; wether an atom is a member of a list
(define member?
  (lambda (a lat)
    (cond
     [(null? lat) #f]
     [(eq? (car lat) a) #t]
     [else (member? a (cdr lat))])))


;; NOTE: The First Commandment
;; Always ask "null?" as the first question in expressing any function

(define rember
  (lambda (a lat)
    (cond
     [(null? lat) (list)]
     [(eq? (car lat) a) (cdr lat)]
     [else (cons (car lat) (rember a (cdr lat)))])))

;; firsts
;; "The function firsts takes one argument, a
;; list, which is either a null list or contains only non-empty lists. It builds
;; another list composed of the first S-expression of each internal list."

(define firsts
  (lambda (l)
    (cond
     [(null? l) (list)]
     [else (cons (car (car l)) (firsts (cdr l)))])))

;; NOTE: Third Commandment
;; When building a list, describe the first typical element,
;; and then "cons" it onto the natural recursion

;; (firsts (list (list 'a 'b) (list 'c 'd)))

(define insertR
  (lambda (new old lat)
    (cond
     [(null? lat) (list)]
     [(eq? (car lat) old) (cons old (cons new (cdr lat)))]
     [else (cons (car lat) (insertR new old (cdr lat)))])))

;; (insertR 'a 'b (list 'x 'z 'b 'y))

(define insertL
  (lambda (new old lat)
    (cond
     [(null? lat) (list)]
     [(eq? (car lat) old) (cons new lat)]
     [else (cons (car lat) (insertL new old (cdr lat)))])))

;; (insertL 'a 'b (list 'x 'z 'b 'y 'b))

;; Hint: (subst new old lat) replaces the first occurrence of old in the lat
;; with new
(define subst
  (lambda (new old lat)
    (cond
     [(null? lat) (list)]
     [(eq? old (car lat))
      (cons new (cdr lat))]
     [else (cons (car lat) (subst new old (cdr lat)))])))

;; (subst 'a 'b (list 'x 'z 'b 'y 'b))


;; Hint:
;; (subst2 new 01 02 lat)
;; replaces either the first occurrence of o1 or the first occurrence of o2 by new
(define subst2
  (lambda (new o1 o2 lat)
    (cond
     [(null? lat) (list)]
     [(eq? (car lat) o1)
      (cons new (cdr lat))]
     [(eq? (car lat) o2)
      (cons new (cdr lat))]
     [else (cons (car lat)
                 (subst2 new o1 o2 (cdr lat)))])))

;; Write the function multirember which gives as its final value the lat with
;; all occurrences of a removed.

(define multirember
  (lambda (a lat)
    (cond
     [(null? lat) (list)]
     [(eq? a (car lat))
      (multirember a (cdr lat))]
     [else (cons (car lat)
                 (multirember a (cdr lat)))])))

(rember 'a (list 'x 'a 'y 'a 'z))

;; TODO: Write multiinsertR, and multiinsertL

(define multiinsertL
  (lambda (new old lat)
    (cond
     [(null? lat) (list)]
     [(eq? (car lat) old)
      (cons new (cons old (multiinsertL new old (cdr lat))))]
     [else (cons (car lat) (multiinsertL new old (cdr lat)))])))

;; (insertL 'a 'b (list 'x 'z 'b 'y 'b))
;; (multiinsertL 'a 'b (list 'x 'z 'b 'y 'b))

(define multiinsertR
  (lambda (new old lat)
    (cond
     [(null? lat) (list)]
     [(eq? (car lat) old)
      (cons old (cons new (multiinsertR new old (cdr lat))))]
     [else (cons (car lat) (multiinsertR new old (cdr lat)))])))

;; (multiinsertR 'a 'b (list 'x 'z 'b 'y 'b))

;; NOTE: Fourth Commandment
;; Always change at least one argument while recurring.
;; It must be changed to be closer to the termination.
;; Changing argument must be tested in the termination condition.
;; Example: when using "cdr", test termination with "null?"

(define multisubst
  (lambda (new old lat)
    (cond
     [(null? lat) (list)]
     [(eq? (car lat) old)
      (cons new (multisubst new old (cdr lat)))]
     [else (cons (car lat) (multisubst new old (cdr lat)))])))

(subst 'a 'b (list 'x 'z 'b 'y 'b))
(multisubst 'a 'b (list 'x 'z 'b 'y 'b))

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

;; Try to write the function o+
;; Hint: It uses zero? add1 and sub1
(define o+
  (lambda (x y)
    (cond
     [(zero? y) x]
     [else (add1 (o+ x (sub1 y)))])))

(define o-
  (lambda (x y)
    (cond
     [(zero? y) x]
     [else (sub1 (o- x (sub1 y)))])))

;; tuple, a list of number, like "atom?"

(define tup?
  (lambda (l)
    (cond
     [(null? l) #t]
     [else (and (number? (car l))
                (tup? (cdr l)))])))

(define addtup
  (lambda (tup)
    (cond
     [(null? tup) 0]
     [else (o+ (car tup)
               (addtup (cdr tup)))])))

;; (addtup (list 1 2 3))

(define o*
  (lambda (n m)
    (cond
     [(zero? m) 0]
     [else (o+ n (o* n (sub1 m)))])))

;; (o* 2 3)

(define tup+
  (lambda (tup1 tup2)
    (cond
     [(null? tup1) tup2]
     [(null? tup2) tup1]
     [else (cons (o+ (car tup1) (car tup2))
                 (tup+ (cdr tup1) (cdr tup2)))])))

;; (tup+ (list 1 2 11) (list 3 5 13 29))

;;Can you write the function > now using zero? and sub1

(define o>
  (lambda (x y)
    (cond
     [(zero? x) #f]
     [(zero? y) #t] ;; as zero is smallest, in Whole number
     [else (o> (sub1 x) (sub1 y))])))

(o> 3 2)
(o> 2 3)
(o> 0 0)

(define o<
  (lambda (x y)
    (cond
     [(zero? y) #f]
     [(zero? x) #t]
     [else (o< (sub1 x) (sub1 y))])))

(define o=
  (lambda (n m)
    (cond
     [(o> n m) #f]
     [(o< n m) #f]
     [else #t])))

(define o^
  (lambda (n m)
    (cond
     [(zero? m) 1]
     [else (o* n (o^ n (sub1 m)))])))

;; (o^ 2 3) ;; 8

(define o/
  (lambda (n m)
    (cond
     [(< n m) 0]
     [else (add1 (o/ (o- n m) m))])))

(define length
  (lambda (lat)
    (cond
     [(null? lat) 0]
     [else (add1 (length (cdr lat)))])))

;; (length (list 'a 'a 'b 'c))

(define pick
  (lambda (n lat)
    (cond
     [(o= n 1) (car lat)]
     [else (pick (sub1 n) (cdr lat))])))

;; (pick 2 (list 'a 'b 'c))

(define rempick
  (lambda (n lat)
    (cond
     [(o= n 1) (cdr lat)]
     [else (cons (car lat)
                 (rempick (sub1 n) (cdr lat)))])))

;; (rempick 2 (list 'a 'b 'c))

(define no-nums
  (lambda (lat)
    (cond
     [(null? lat) (list)]
     [(number? (car lat))
      (no-nums (cdr lat))]
     [else (cons (car lat)
                 (no-nums (cdr lat)))])))

;; (no-nums (list 'a 1 'b 3 'c))

(define all-nums
  (lambda (lat)
    (cond
     [(null? lat) (list)]
     [(not (number? (car lat)))
      (all-nums (cdr lat))]
     [else (cons (car lat)
                 (all-nums (cdr lat)))])))

;; (all-nums (list 'a 1 'b 3 'c))

(define eqan?
  (lambda (a1 a2)
    (cond
     [(and (number? a1) (number? a2)) (o= a1 a2)]
     [else (eq? a1 a2)])))

;; (eqan? 'a 'a)

(define occur
  (lambda (a lat)
    (cond
     [(null? lat) 0]
     [(eqan? a (car lat))
      (add1 (occur a (cdr lat)))]
     [else (occur a (cdr lat))])))

;; (occur 'a (list 'a 'a 'b 'a))
(occur 1 (list 'a 1 'b 1))

(define one?
  (lambda (n)
    (eqan? n 1)))

;; (one? 1)

;; like multirember that works recursively inwards
(define rember*
  (lambda (a l)
    (cond
     [(null? l) (list)]
     [(atom? (car l))
      (cond
       [(eq? a (car l))
        (rember* a (cdr l))]
       [else (cons (car l)
                   (rember* a (cdr l)))])]
     [else (cons (rember* a (car l))
                 (rember* a (cdr l)))])))

;; (rember* 'a (list 'a (list 'a 'b) 'b 'a))

(define insertR*
  (lambda (new old l)
    (cond
     [(null? l) (list)]
     [(atom? (car l))
      (cond
       [(eq? old (car l))
        (cons old
              (cons new
                    (insertR* new old (cdr l))))]
       [else (cons (car l)
                   (insertR* new old (cdr l)))])]
     [else (cons (insertR* new old (car l))
                 (insertR* new old (cdr l)))])))

;; (insertR* 1 2 (list 2 3 4 2 6 2))

;; how many times 'a' occurs in 'l'
(define occur*
  (lambda (a l)
    (cond
     [(null? l) 0]
     [(atom? (car l))
      (cond
       [(eq? a (car l))
        (add1 (occur* a (cdr l)))]
       [else (occur* a (cdr l))])]
     [else (o+ (occur* a (car l))
               (occur* a (cdr l)))])))

;; (occur* 1 (list 1 2 (list 3 1) (list (list 1 1))))

(define subst*
  (lambda (new old l)
    (cond
     [(null? l) (list)]
     [(atom? (car l))
      (cond
       [(eq? old (car l))
        (cons new (subst* new old (cdr l)))]
       [else (cons (car l)
                   (subst* new old (cdr l)))])]
     [else (cons (subst* new old (car l))
                 (subst* new old (cdr l)))])))

;; (subst* 1 2 (list 2 3 2 4 (list 5 2 (list 7 2))))

(define insertL*
  (lambda (new old l)
    (cond
     [(null? l) (list)]
     [(atom? (car l))
      (cond
       [(eq? old (car l))
        (cons new
              (cons old
                    (insertL* new old (cdr l))))]
       [else (cons (car l)
                   (insertL* new old (cdr l)))])]
     [else (cons (insertL* new old (car l))
                 (insertL* new old (cdr l)))])))

;; (insertL* 1 2 (list 2 3 4 2 6 2))

(define member*
  (lambda (a l)
    (not (zero? (occur* a l)))))

;; (member* 1 (list (list 1)))

;; return the left most atom, only for non-empty
(define leftmost
  (lambda (l)
    (cond
     [(atom? (car l)) (car l)]
     [else (leftmost (car l))])))

(define eqlist?
  (lambda (l1 l2)
    (cond
     [(and (null? l1) (null? l2)) #t]
     [(and (null? l1) (atom? (car l2))) #f]
     [(null? l1) #f]
     [(and (atom? (car l1)) (null? l2)) #f]
     [(and (atom? (car l1)) (atom?  (car l2)))
      (and (eqan? (car l1) (car l2))
           (eqlist? (cdr l1) (cdr l2)))]
     [(atom? (car l1)) #f]
     [(null? l2) #f]
     [(atom? (car l2)) #f]
     [else (and (eqlist? (car l1) (car l2))
                (eqlist? (cdr l1) (cdr l2)))])))

;; (eqlist? (list 1 (list 2) 3) (list 1 (list 2) 3))

(define equal?
  (lambda (s1 s2)
    (cond
     [(and (atom? s1) (atom? s2))
      (eqan? s1 s2)]
     [(or (atom? s1) (atom? s2)) #f]
     [else (eqlist? s1 s2)])))


(define rember
  (lambda (s l)
    (cond
     [(null? l) (list)]
     [(equal? (car l) s) (cdr l)]
     [else (cons (car l) (rember s (cdr l)))])))

;; (rember (list 1) (list 1 2 (list 1)))

(define numbered?
  (lambda (aexp)
    (cond
     [(atom? aexp) (numer? aexp)]
     [else (and (numbered? (car aexp))
                (numbered? (car (cdr (cdr aexp)))))])))


(define value
  (lambda (nexp)
    (cond
     [(atom? nexp) nexp]
     [(equal? (car nexp) (quote +)) (o+ (value (car (cdr nexp)))
                                        (value (car (cdr (cdr nexp)))))]
     [else (o^ (value (car (cdr nexp)))
               (value (car (cdr (cdr nexp)))))])))

;; (value (list (quote ^) 2 3))

(define 1st-sub-exp
  (lambda (nexp)
    (car (cdr nexp))))

(define 2nd-sub-exp
  (lambda (nexp)
    (car (cdr (cdr nexp)))))

(define operator
  (lambda (nexp)
    (car nexp)))

(define value
  (lambda (nexp)
    (cond
     [(atom? nexp) nexp]
     [(eq? (operator nexp) (quote +))
      (o+ (1st-sub-exp nexp)
          (2nd-sub-exp nexp))]
     [else (o^ (1st-sub-exp nexp)
               (2nd-sub-exp nexp))])))

(value (list (quote +) 2 3))


;; for a new representation of counting
(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons (list) n)))

(define zub1
  (lambda (n)
    (cdr n)))

;; Chapter 7
(define member?
  (lambda (a lat)
    (cond
     [(null? lat) #f]
     [(equal? (car lat) a) #t]
     [else (member? a (cdr lat))])))

;; list of atoms, where no atom repeats
(define set?
  (lambda (lat)
    (cond
     [(null? lat) #t]
     [(member? (car lat) (cdr lat)) #f]
     [else (set? (cdr lat))])))

;; removes the duplicates
(define makeset
  (lambda (lat)
    (cond
     [(null? lat) lat]
     [(member? (car lat)
               (cdr lat))
      (makeset (cdr lat))]
     [else (cons (car lat)
                 (makeset (cdr lat)))])))

(define subset?
  (lambda (set1 set2)
    (cond
     [(null? set1) #t]
     [(member? (car set1) set2)
      (subset? (cdr set1) set2)]
     [else #f])))


(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))

;; (eqset? (list 1 2 3) (list 1 2 3))

;; at least one in set1 is present in set2
(define intersect?
  (lambda (set1 set2)
    (cond
     [(null? set1) #f]
     [else (or (member? (car set1) set2)
               (intersect? (cdr set1) set2))])))

(define intersect
  (lambda (set1 set2)
    (cond
     [(null? set1) (list)]
     [(member? (car set1) set2)
      (cons (car set1)
            (intersect (cdr set1) set2))]
     [else (intersect (cdr set1) set2)])))

;; (intersect (list 1 2 3) (list 2 3 4))

(define union
  (lambda (set1 set2)
    (cond
     [(null? set1) set2]
     [(member? (car set1) set2)
      (union (cdr set1) set2)]
     [else (cons (car set1)
                 (union (cdr set1) set2))])))

(define intersectall
  (lambda (l-set)
    (cond
     [(null? (cdr l-set)) (car l-set)]
     [else (intersect (car l-set)
                      (intersectall (cdr l-set)))])))

;; (intersectall (list (list 1 2 3) (list 2 3 4) (list 3 4 5)))

(define a-pair?
  (lambda (x)
    (cond
     [(atom? x) #f]
     [(null? x) #f]
     [(null? (cdr x)) #f]
     [(null? (cdr (cdr x))) #t]
     [else #f])))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define build
  (lambda (s1 s2)
    (cons s1
          (cons s2
                (list )))))

(define third
  (lambda (s-list)
    (car (cdr (cdr s-list)))))


(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define revrel
  (lambda (rel)
    (cond
     [(null? rel) (list)]
     [else (cons (build (second (car rel))
                        (first (car rel)))
                 (revrel (cdr rel)))])))

;; (revrel (list (list 1 2) (list 3 4)))

(define revpair
  (lambda (pair)
    (build (second pair)
           (first pair))))

(define revrel
  (lambda (rel)
    (cond
     [(null? rel) (list)]
     [else (cons (revpair (car rel))
                 (revrel (cdr rel)))])))

(define seconds
  (lambda (rel)
    (cond
     [(null? rel) (list)]
     [else (cons (second (car rel))
                 (seconds (cdr rel)))])))

(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))

;; (fullfun? (list (list 2 3) (list 5 3)))

;; a different way to write "fullfun?"
(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))))

(define rember-f
  (lambda (test? a l)
    (cond
     [(null? l) (list)]
     [(test? (car l) a)
      (cdr l)]
     [else (cons (car l)
                 (rember-f test? a (cdr l)))])))

;; Chapter 8

;; NOTE: currying (Thank you Haskell B. Curry)
(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? a x))))

(define eq?-salad
  (eq?-c 'salad))

;; (eq?-salad 'salad)

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
       [(null? l) (list)]
       [(test? (car l) a)
        (cdr l)]
       [else (cons (car l)
                   ((rember-f test?) a l))]))))

;; ((rember-f eq?) 1 (list 1 2 3))


(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
       [(null? l) (list)]
       [(test? old (car l))
        (cons new (cons old (cdr l)))]
       [else (cons (car l)
                   ((insertL-f test?) new old (cdr l)))]))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
       [(null? l) (list)]
       [(test? old (car l))
        (cons old (cons new (cdr l)))]
       [else (cons (car l)
                   ((insertR-f test?) new old l))]))))

(define insert-g
  (lambda (direction)
    (lambda (test?)
      (lambda (new old l)
        (cond
         [(null? l) (list)]
         [else (cond
                [(eq? direction 'left)
                 ((insertL-f test?) new old l)]
                [else ((insertR-f test?) new old l)])])))))

;; (((insert-g 'right) eq?) 1 2 (list 2 3 4))
;; (((insert-g 'left) eq?) 1 2 (list 2 3 4))

(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))


(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
       [(null? l) (list)]
       [(eq? (car l) old)
        (seq new old (cdr l))]
       [else (cons (car l)
                   ((insert-g seq) new old (cdr l)))]))))

(define insertL (insert-g seqL))
(define insertR (insert-g seqR))

(insertL 1 2 (list 3 4 5 1 2))

(define seqS
    (lambda (new old l)
    (cons new l)))

(define subst (insert-g seqS))

(subst 1 2 (list 3 4 2))

(define atom-to-function
  (lambda (x)
    (cond
     [(eq? x (quote +)) o+]
     [(eq? x (quote -)) o-]
     [else o^])))

(define value
  (lambda (nexp)
    (cond
     [(atom? nexp) nexp]
     [else ((atom-to-function (operator nexp))
            (1st-sub-exp nexp)
            (2nd-sub-exp nexp))])))

;; (value (list (quote +) 2 3))

;; write mutlirember-f: page no. 135
(define multirember-f
  (lambda (test)
    (lambda (a l)
      (cond
       [(null? l) (list)]
       [(test a (car l))
        ((multirember-f test) a (cdr l))]
       [else (cons (car l)
                   ((multirember-f test) a (cdr l)))]))))

((multirember-f eq?)'a (list 'x 'a 'y 'a 'z))

(define multirember-eq?
  (multirember-f eq?))

;; page number 127
(define eq?-c
  (lambda (a)
    (lambda (b)
      (eq? a b))))

(define eq?-tuna
  (eq?-c 'tuna))

;; (eq?-tuna 'tuna) ;; #t


;; NOTE: Am I learning about generics?
;; "test?" here is representative for something like eq?-tuna
(define multiremberT
  (lambda (test? lat)
    (cond
     [(null? lat) (list)]
     [(test? (car lat))
      (multiremberT test? (cdr lat))]
     [else (cons (car lat)
                 (multiremberT test? (cdr lat)))])))

;; (multiremberT eq?-tuna (list 1 2 'tuna 4 'tuna 'buna))


;; NOTE: This is the start of "collectors", or "continuation"

;; NOTE: Tenth commandment:- Build functions to collect more than one value at a
;; time

;; Now write the function evens-only* which removes all odd numbers from a list
;; of nested lists. Here is even?


(define evens-only*
  (lambda (l)
    (cond
     [(null? l) l]
     [(atom? (car l))
      (cond
       [(even? (car l))
        (cons (car l)
              (evens-only* (cdr l)))]
       [else (evens-only* (cdr l))])]
     [else (cons (evens-only* (car l))
                 (evens-only* (cdr l)))])))

(evens-only* (list 1 2 3 (list 4 6 (list 3 8)) 8 10 13 (list)))

;; NOTE: Probably the most important section of the book
;; The end of the Chapter 8 didn't make much sense.
;; I can understand what it was trying to do, but not make sense of the syntax

;; NOTE: Chapter 9

;; (define looking
;;   (lambda (a lat col)
;;     (cond
;;      [(null? lat) #f]
;;      [(number? (car l)) #t]
;;      [else])))

;; unable to think how I will traverse the list while keeping a track
;; I'll need a separate variable for this

;; (looking 'caviar (list 6 2 'grits 'caviar 5 7 3))

;; writing a helper func that get's the aton at index "n"
;; assuming an atom exists at "n"
(define get-atom-at-n
  (lambda (n lat) ;; need to reduce n, by 1, till 1
    (cond
     [(eq? n 1) (car lat)]
     [else (get-atom-at-n (o- n 1) (cdr lat))])))

(get-atom-at-n 2 (list 4 5 6 7))

;; I realized I have already written a similar function called "pick"

;; (define keep-looking
;;   (lambda (a b lat)
;;     (cond
;;      [(eq? a b) #t]
;;      [else (keep-looking a (pick b lat) lat)])))

;; (keep-looking 'caviar
;;               (pick 1 (list 6 2 4 'caviar 5 7 3))
;;               (list 6 2 4 'caviar 5 7 3))

;; (define keep-looking ;; example of unnatural recursion (does not recurd on the part of "lat")
;;   (lambda (a sorn lat) ;; sorn => symbol or number
;;     (cond
;;      [(number? sorn) ;; if number, go to that location
;;       (keep-looking a (pick sorn lat) lat)]
;;      [else (eq? a sorn)]))) ;; if not number, check equality

;; NOTE: Beginning to realize that, to use "col", I need a function inside a
;; function. Inner function takes one extra argument, which is col

;; (define looking
;;   (lambda (a lat)
;;     (keep-looking a (pick 1 lat) lat)))

;; (looking 'caviar (list 6 2 4 'caviar 5 7 3))

;; a function that never stops recurring

;; NOTE: Partial functions, that recur on the part of the whole, and thus
;; eventually comes to a conclusion

;; Here's an example of a non-partial function
;; Partial vs total. Following is the most partial function
(define eternity
  (lambda (x)
    (eternity x)))

;; (shift ((a b) c)) -> (a (b c))
;; (shift ((a b) (c d))) -> (a (b (c d)))

;; (define shift
;;   (lambda (pair)
;;     (build (first (first pair))
;;            (build (second (first pair))
;;                   (second pair)))))

;; (shift (list (list 'a 'b) 'c))
;; (shift (list (list 'a 'b) (list 'c 'd)))

(define align
  (lambda (pora)
    (cond
     [(atom? pora) pora]
     [(a-pair? pora)
      (align (shift pora))]
     [else (build (build (first pora)
                         (align (second pora))))])))

(define length*
  (lambda (pora)
    (cond
     [(atom? pora) 1]
     [else (o+ (length* (first pora))
               (length* (second pora)))])))

(define weight*
  (lambda (pora)
    (cond
     [(atom? pora) 1]
     [else
      (o+ (o* (weight* (first pora)) 2)
          (weight* (second pora)))])))

(weight* (list (list 'a 'b) 'c))

;; Chapter 10

(define lookup-in-entry
  (lambda (name entry entry-f) ;; entry-f is used when "name" does not exist in "entry"
    (lookup-in-entry-help name
     (first entry)
     (second entry)
     entry-f)))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
     [(null? names) (entry-f name)]
     [(eq? (car names) name)
      (car values)]
     [else (lookup-in-entry-help name
                                 (cdr names)
                                 (cdr values)
                                 entry-f)])))

;; NOTE: Revisiting Chapter 8 because somewhere along the lines I lost the plot

;; like "rember", but you have to provice the func
(define rember-f
  (lambda (test? a l)
    (cond
     [(null? l) (list)]
     [(test? a (car l)) ;; if the condition is met
      (rember-f test? a (cdr l))] ;; then continue to apply condition on rest
     [else (cons (car l)
                 (rember-f test? a (cdr l)))])))

(rember-f eq? 1 (list 1 2 3 (list 4 3 1)))


;; (define fibSum
;;   (lambda (n)
;;     (lambda (n)
;;       (cond
;;        [(zero? n) 0]
;;        [(one? n) 1]
;;        [else (o+ fibSum(o- n 1)
;;                  fibSum(o- n 2))]))))



;; get the Nth Fib number
(define fibN
  (lambda (n)
    (cond
     [(< n 2) n]
     [else (+ (fibN(- n 2))
               (fibN(- n 1)))])))

;; sum of N Fib numbers
;; (define fibSum
;;   (lambda (n sum)
;;     (lambda (n sum 0)
;;       (cond
;;        [(< n 2) n]
;;        [else (fibSum (fibSum ()))])))





;; Procedures can take a callback to invoke upon their return value.

;; direct style of summing list of numbers
(define sum-of-list
  (lambda (lat)
    (cond
     [(null? lat) 0] ;; NOTE base case
     [else (+ (car lat)
              (sum-of-list (cdr lat)))])))

(sum-of-list (list 1 2 3 5))

(define sum-of-list-cps
  (lambda (lat k)
    (cond
     [(null? lat) (k 0)]
     [else (sum-of-list-cps (cdr lat)
                            (lambda (the-answer-from-the-rest)
                              (k (+ (car lat) the-answer-from-the-rest))))])))

(sum-of-list-cps (list 1 2 3 5) (lambda (x) x))

;; NOTE Factorial direct style
(define factorial
  (lambda (n)
    (cond
     [(= n 0) 1]
     [else (* n
              (factorial (- n 1)))])))

(factorial 3)

;; Continuation Passing Style
(define factorial-cps
  (lambda (n k)
    (cond
     [(zero? n) (k 1)]
     [else (factorial-cps (- n 1)
                          (lambda (the-answer-from-the-rest)
                            (k (* n the-answer-from-the-rest))))])))
;; NOTE "n" in the lambda

(factorial-cps 3 (lambda (x) x))

;; NOTE: Not from the book
;; writing a function that sums a list but instantly stops and returns 0 if it
;; finds a 0 anywhere in the list? (This is where CPS becomes a superpower!)

(define sumTillZero ;; computation of the sum, should happen in lambda
  (lambda (lat k)
    (cond
     [(null? lat) (k 0)]
     [(zero? (car lat)) 0]
     [else (sumTillZero (cdr lat)
                        (lambda (rest)
                          (k (+ (car lat) rest))))])))

(sumTillZero (list 1 2 3 13) (lambda (x) x))

;; Now I wanted to again work on the the CPS for multirember
(define multirember-k
  (lambda (a lat k)
    (cond
     [(null? lat) (k (list))]
     [(not (eq? a (car lat)))
      (multirember-k a (cdr lat)
                       (lambda (l)
                         (k (cons (car lat) l))))]
     [else (multirember-k a (cdr lat) k)])))

(multirember-k 1 (list 3 2 1 4 5 1 5 1) (lambda (x) x))

;; "building the result on the way back up" by wrapping the continuation k in
;; another lambda.

;; NOTE: This is where I found LLMs helpful
;; They helped generate question/exercises for me to solve
;; To test my understanding of the CPS

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat))) ;; (pick 1 lat), basically returns first element
                                        ;; could we have gone with "(car lat)" ?

(define keep-looking
  (lambda (a sorn lat) ;; sorn = symbol or number
    (cond
     [(number? sorn)
      (keep-looking a (pick sorn lat) lat)]
     [else (eq? a sorn)])))

(looking 'a (list 3 2 'b))

;; here, with looking and keep-looking, the book is trying to teach me
;; about the inner lambda func that I have to use in CPS

(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))

;; (shift (list (list 'a 'b) 'c))

;; first lets build an entry
(define new-entry build)

(define e (new-entry (list 'a 'b 'c)
           (list 'x 'e 'z)))

;; (display e)

;; NOTE this assumed that name exists in the entry
(define look-in-entry
  (lambda (name entry)
    (cond
     [(eq? name (first (first entry)))
      (first (second entry))]
     [else (look-in-entry name (new-entry (cdr (first entry))
                                          (cdr (second entry))))])))

(look-in-entry 'c e)

;; Book wants me to write a version that takes a function which gets called
;; when the "name" does not exist in the entry

(define look-in-entry
  (lambda (name entry entry-f)
    (look-in-entry-help name
                        (first entry)  ;; names
                        (second entry) ;; values
                        entry-f)))     ;; the function that gets called when "name" not in entry


(define look-in-entry-help
  (lambda (name names values entry-f)
    (cond
     [(null? names) (entry-f name)]
     [(eq? name (car names)) (car values)]
     [else (look-in-entry-help name
                               (cdr names)
                               (cdr values)
                               entry-f)])))

;; being able to implement this with my own self was fun
(define look-in-table
  (lambda (name table table-f)
    (cond
     [(null? table) (table-f name)]
     [else (look-in-entry name
                          (car table)
                          (lambda (name)
                            (look-in-table name
                                (cdr table)
                                table-f)))])))


;; end segment
(define expression-to-action
  (lambda (e)
    (cond
     [(atom? e) (atom-to-action e)]
     [else (list-to-action e)])))

