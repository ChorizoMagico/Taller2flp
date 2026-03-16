#lang eopl

(provide (all-defined-out))
(require "ejercicio1.rkt")


;; Integrantes:
;; Juan Esteban Arias Saldaña (2417915)
;; Valeria Zamudio Arevalo (2415210)

;;2.1
;; PARSEBNF :
;; Proposito:
;; L -> L': Recibe una lista lista con la representacion concreta de una
;; instancia SAT (con simbolos 'or y 'and explicitos) y construye el arbol
;; de sintaxis abstracta basado en listas, eliminando los simbolos 'or y 'and.
;; <lista> := (FNC <número> ((<clausulaOR> and)* <clausulaOR>))
(define PARSEBNF (lambda (lista) (letrec (


                                      (parseOR (lambda (lista)
                                                  (cond
                                                    [(null? lista) '()]
                                                    [(number? (car lista)) (cons (car lista) (parseOR (cdr lista)))]
                                                    [else (parseOR (cdr lista))]
                                                    )
                                                  ))
                                      
                                      (parseAND (lambda (lista)
                                                  (cond
                                                    [(null? lista) '()]
                                                    [(list? (car lista)) (cons (parseOR (car lista)) (parseAND (cdr lista)))]
                                                    [else (parseAND (cdr lista))]
                                                    )
                                                  ))
                                      [parseInterno (lambda (lista)
                                                      (cond
                                   [(equal? (car lista) 'FNC) (cons 'FNC (parseInterno (cdr lista)))]
                                   [(number? (car lista)) (cons (car lista) (parseInterno (cdr lista)))]
                                   [else (list (parseAND (car lista)))]
                                   ))]
                                                      )
                                      

                                    
                                 (if (not (is-FNC? lista))(eopl:error 'PARSEBNF "no es una FNC valida") (parseInterno lista)
                                 )
                                 )))

;; Pruebas PARSEBNF
(PARSEBNF '(FNC 2 ((1 or -2) and (-1 or 2))))
;; > (FNC 2 ((1 -2) (-1 2)))
(PARSEBNF '(FNC 3 ((1 or 2 or 3) and (-1 or 2) and (1 or -3))))
;; > (FNC 3 ((1 2 3) (-1 2) (1 -3)))
(PARSEBNF '(FNC 1 ((1))))
;; > (FNC 1 ((1)))    
                       


;; 2.2

;; unir :
;; Proposito:
;; L1 x L2 -> L’: Une dos listas en una sola.
;;
;; <listas> := ()
;;           := (<valor-de-scheme> <listas>)

(define unir (lambda (L1 L2)
  (if (null? L1) L2
      (cons (car L1) (unir (cdr L1) L2)))))

;; Pruebas unir
(unir '(1 2 3) '(4 5 6))
(unir '() '(1 2 3))
(unir '(a b) '(c d e))




;; UNPARSEBNF-OR :
;; Proposito:
;; L -> L': Recibe una clausula exp (lista de literales enteros).
;; Devuelve la clausula con el simbolo 'or intercalado entre cada número.
;; <exp> := (<número-de-racket>)
;;       := (<número-de-racket> <exp>)

(define UNPARSEBNF-OR (lambda (exp)
                                   (cond
                                     [(null? exp) '()]
                                     [(not (number? (car exp))) (eopl:error 'UNPARSEBNF-OR "no es una lista de numeros")]
                                     [(null? (cdr exp)) (list (car exp))]
                                     [else (unir (list (car exp) 'or) (UNPARSEBNF-OR (cdr exp)))])))
  

;; Pruebas UNPARSEBNF-OR
(UNPARSEBNF-OR '(1 2 3))
(UNPARSEBNF-OR '(-1 2))
(UNPARSEBNF-OR '(1))


;; UNPARSEBNF-AND :
;; Proposito:
;; L -> L': Recibe una FNC exp (lista de clausulas de números enteros).
;; Devuelve la FNC con cada clausula convertida por UNPARSEBNF-OR
;; y el simbolo 'and intercalado entre cada clausula.
;; <exp> := ((<número-de-racket> ...))
;;       := ((<número-de-racket> ...) <exp>)

(define UNPARSEBNF-AND (lambda (exp)
                                    (cond
                                      [(null? (cdr exp)) (list (UNPARSEBNF-OR (car exp)))]
                                      [else (unir (list (UNPARSEBNF-OR (car exp)) 'and) (UNPARSEBNF-AND (cdr exp)))]
                                     )))

;; Pruebas UNPARSEBNF-AND
(UNPARSEBNF-AND '((1 2)(3 4)))
(UNPARSEBNF-AND '((1 -2 3)))
(UNPARSEBNF-AND '((1)(2 3)(-1 -2)))



;; UNPARSEBNF :
;; Proposito:
;; L -> L': Recibe una expresion exp en formato interno
;; (FNC <num-variables> <lista-de-clausulas>).
;; Verifica que sea una FNC valida y devuelve la representacion
;; legible con 'or y 'and intercalados entre literales y clausulas.
;; <exp> := (FNC <número> <lista-de-clausulas>)

(define UNPARSEBNF (lambda (exp)
                                (if (and (equal? (car exp) 'FNC) (number? (cadr exp)) (list? (caddr exp)))
                                    (list 'FNC (cadr exp) (UNPARSEBNF-AND (caddr exp)))
                                (eopl:error 'UNPARSEBNF "no es una FNC valida"))))
  
;; Casos de prueba
(UNPARSEBNF '(FNC 3 ((1 -2 3) (-1 2))))
(UNPARSEBNF '(FNC 2 ((1 2) (-1 -2) (1 -2))))
(UNPARSEBNF '(FNC 4 ((1 2 3) (-1 -2) (2 -3 4) (-4 1))))