; 1. Write a function to check if an atom is member of a list (the list is non-liniar)

(defun map_member(E L)
  (cond
   ((null L) NIL)
   ((and (atom L) (equal L E)) T)
   ((and (atom L) (not (equal L E))) NIL)
   (T (eval (cons 'or (mapcar #'(lambda(X) (map_member E X)) L))))
  )
)

(print (map_member 4 '(2 3 (4 5) 6)))

; 2. Write a function that returns the sum of numeric atoms in a list, at any level.

(defun sum (l)
    (cond 
        ((null l) 0)
        ((numberp l) l)
        ((atom l) 0)
        (T (apply #' +(mapcar #'sum l)))))
        
;(print (sum '(2 3 (4 5) 6)))

; 5. Write a function that computes the sum of even numbers and the decrease the sum of odd numbers,
; at any level of a list.

(defun p (l)
(cond
    ((null l) 0)
    ((and(numberp l) (equal (mod l 2) 0)) l)
    ((and(numberp l) (equal (mod l 2) 1)) (* l -1))
    ((atom l) 0)
    (t (apply #' +(mapcar #'p l)))))
    

;(print (p '(2 3 (4 5) 6)))

; 6. Write a function that returns the maximum of numeric atoms in a list, at any level
(defun macsim (l)
    (cond
        ((null l) nil)
        ((numberp l) l)
        ((atom l) -1000)
        (t (apply #' max(mapcar #'macsim l)))))

;(print (macsim '(2 3 (4 5) 6)))


; 7. Write a function that substitutes an element E with all elements of a list L1 at all levels of a given list L.

(defun copy_list(L)
  (cond
   ((null L) nil)
   (T (cons (car L) (copy_list (cdr L))))
  )
)

(defun subst2(E L1 L)
  (cond
   ((null L) nil)
   ((and (atom L) (equal L E)) (copy_list L1))
   ((atom L) (list L))
   (T (list (mapcan #'(lambda(X) (subst2 E L1 X)) L)))
  )
)

(defun subst1(E L1 L)
  (car (subst2 E L1 L))
)

;1. Sa se construiasca o functie care intoarce adancimea unei liste.

;2. Definiti o functie care obtine dintr-o lista data lista tuturor atomilor
; care apar, pe orice nivel, dar in aceeasi ordine. De exemplu
; (((A B) C) (D E)) --> (A B C D E)

;11. Sa se scrie o functie care sterge toate aparitiile unui atom de la
; toate nivelurile unei liste.

(defun adanc (l)
    (cond
        ((null l) 0)
        ((atom l) 0)
        (t (+ 1(apply #' max(mapcar #' adanc l))))))
        
       ; (print (adanc '(2 3 (4 5 (6)) (4 5) 6)))
       
(defun doi (l)
    (cond
        ((null l) nil)
        ((atom l) (copy_list (list l)))
        (t (mapcan(lambda (x) (doi x))l))))
        
;(print (doi '(2 3 (4 5 (6)) (4 5) 6)))
 
(defun unspe (l e)
    (cond
        ((null l) nil)
        ((and(atom l) (equal l e)) l)
        ((atom l) (copy_list (list l)))
        (t (list(mapcan (lambda(X) (unspe X e)) L)))))
       
(print (car(unspe '(2 3 (4 5 (4)) (4 5) 6) 4)))

;16. Definiti o functie care inverseaza o lista impreuna cu toate sublistele
; sale de pe orice nivel.

(defun my_reverse (l)
    (cond
        ((null l) nil)
        (t (append (my_reverse (cdr l)) (list(car l))))))
        
;(print (my_reverse '(2 3 4 (5 6 7) 8 9)))

(defun nivelk (l n k e)
    (cond
        ((null l) nil)
        ((and (atom l) (equal n k)) (list e))
        ((atom l) (list l))
        (t (list(mapcan (lambda(X) (nivelk X (+ n 1) k e))l)))))

(print (nivelk '(A (B) (C (D) (E))) 0 2 'Q))

(defun miniim (l m)
    (cond
        ((null l) m)
        ((and(numberp (car l)) (< (car l) m)) (miniim (cdr l) (car l)))
        ((numberp (car l)) (miniim (cdr l) m))
        ((listp (car l)) (min (miniim (car l) m) (miniim (cdr l) m)))
))

(defun fuc(l n)
    (cond
    ((null l) 0)
    ((atom l) 0)
    ((and (equal (mod n 2) 1) (equal (mod (miniim l 4444) 2) 0)) (apply #' + 1 (mapcar (lambda (X) (fuc X (+ n 1))) l )))
    (t (apply #' +(mapcar (lambda (X) (fuc X (+ n 1)))l)))))
    
;(print (fuc '(2 3 4 (2 4 1 ( 7 8 4 (7)) 8 9)) 1))

(defun pr(l e)
    (cond 
        ((null l) nil)
        ((listp (car l)) (cons (pr (car l) e) (pr (cdr l) e)))
        ((equal (car l) e) (pr (cdr l) e))
        (t (cons (car l) (pr (cdr l) e)))))
        
;(print (pr '(2 4 3 4 (2 3 5 1 2 (3 2) (2))) 2))



(defun stav(l e n)
    (cond
        ((null l) nil)
        ((equal (mod n 2) 1) (append (append (list(car l)) (list e)) (stav (cdr l) e (- n 1))))
        (t (cons (car l) (stav (cdr l) e (- n 1))))))
        
 ;(print (stav '(1 2 3 4 5 6 6 7 8) 20 0))

(defun verific (l)
    (cond 
        ((numberp (car l)) 0)
        (t 1)))
        
(defun pro (l)
(cond
    ((null l) nil) 
    ((atom l) 0)
    ((and (listp l) (equal (verific l) 1)) (apply #' + 1 (mapcar (lambda(X) (pro X))l)))
    (t (apply #' + (mapcar (lambda(X) (pro X))l)))))
    
; (print (pro '(2 3 2 (2 3 f a) ((a) j 4) (2 g))))


;5. Se da o lista neliniara. Sa se scrie un program LISP pentru determinarea numarului de subliste de la orice nivel pentru care primul atom numeric (la orice nivel) este impar. Prelucrarea se va face folosind o functie MAP.

(defun contains (l)
    (cond 
        ((null l) nil)
        ((numberp (car l)) t)
        ((listp (car l)) (or (contains (car l)) (contans (cdr l))))
        (t (contains (cdr l)))))


(defun ch (l)
(cond
    ((null l) nil)
    ((and (numberp (car l)) (equal (mod (car l) 2) 1)) t)
    ((numberp (car l)) nil)
    ((and (listp (car l)) (contains (car l))) (ch (car l)))
    (t (ch (cdr l)))))
    
  
    
    
 (defun mappp(l)
 (cond
    ((null l) 0)
    ((atom l) 0)
    ((and (listp l) (chestie l)) (apply #' + 1 (mapcar #' mappp l)))
    (t (apply #' + (mapcar #' mappp l)))))

    
  ;   (print (mappp '(a 3 (b 2) (1 c 4) (d 2 (6 f))((g 4) 6))))
 ;   (print (mappp '(a c (e f (2 3 2) v (a (5)) 2))))
    
 ;3. Se da o lista neliniara. Sa se scrie un program LISP pentru determinarea numarului de subliste de la orice nivel pentru care numarul atomilor numerici de la orice nivel este impar - nivelul superficial al listei se considera 1. Prelucrarea se va face folosind o functie MAP.
 
 
 
 
 
 (defun verifi (l)
    (cond
        ((null l) 0)
        ((numberp (car l)) (+ 1 (verifi (cdr l))))
        ((atom (car l) ) (verifi (cdr l)))
        (t (+ (verifi (car l)) (verifi (cdr l))))))
        
   ;(print (verifi '(2 3 (s v 5 1 d ( t 5)) 3 ))) 
   
   
   (defun mapp (l)
    (cond 
        ((null l) 0)
        ((atom l) 0)
        ((and (listp l) (equal (mod (verifi l) 2) 1)) (apply #' + 1 (mapcar #' mapp l)))
        (t (apply #' + (mapcar #' mapp l)))))
        
       ; (print (mapp '(2 3 (s v 5 1 d ( t 5)) 3 1)))
       
 ;  5. Se da o lista neliniara. Sa se scrie un program LISP pentru determinarea numarului de subliste de la orice nivel pentru care atomul numeric maxim de pe nivelurile impare este par - nivelul superficial al listei se considera 1. Prelucrarea se face folosind o functie MAP.    
       
       
    (defun maxx (l)
    (cond
        ((null l) 0)
        ((numberp (car l)) (max (car l) (maxx (cdr l))))
        ((listp (car l)) (maxx (cdr l)))
        ((atom (car l)) (maxx (cdr l))))
        )
        
       ; (print (maxx '(2 3 (s v 5 1 d ( t 5)) 3 1)))
        
  (defun b (l n)
  (cond
        ((null l) 0)
        ((atom l) 0)
        ((and (equal(mod n 2) 1) (equal (mod (maxx l) 2) 0)) (apply #' + 1 (mapcar (lambda (x) (b x (+ n 1)))l)))
        (t (apply #' + (mapcar (lambda (x) (b x (+ n 1)))l)))))
        
      ;(print (b '(2 4 (s v 5 1 d ( t 6)) 3 1) 1))  
        
        
   (defun ff(l)
        t (cdr l))
        
    (print (append (ff '(1 2)) '(3 4 5)))
             
 