#lang eopl

;; Integrantes:
;; Juan Esteban Arias Saldaña (2417915)
;; Valeria Zamudio Arevalo (2415210)

;;1.1


;;Gramática:
;;  <FNC> := FNC <número-de-racket> (<clausulasAND>)
;;
;;  <clausulaOR> := <var> or <clausulaOR>
;;               := <var>
;;
;;  <clausulasAND> := (<clausulaOR>) and <clausulasAND>
;;                 := (<clausulaOR>) 
;;
;; <var> := <número-de-racket>


;;var
(define var (lambda (sym) 
                              sym
                              ))

(define is-var? (lambda (sym) (if
                                  (number? sym) #t #f
                                  )))

(define fnc->var (lambda (FNC var) 
                                     (cond
                                       [(null? FNC) #f]
                                       [(list? (car FNC)) (or (fnc->var (car FNC) var) (fnc->var (cdr FNC) var))]
                                       [(and (is-var? (car FNC)) (equal? (car FNC) var)) #t]
                                       [else (fnc->var (cdr FNC) var)]
                                       )))

;;clausulaOR
(define clausulaOR (lambda (listVar)
                   (cond
                     [(null? listVar) '()]
                     [(null? (cdr listVar)) (list (var (car listVar)))]
                     [else (cons (var (car listVar)) (cons 'or (clausulaOR (cdr listVar))))]
                     )
                   ))

(define is-clausulaOR? (lambda (clausulaOR) (letrec
                                                (
                                                 [recorrerOR (lambda (clausulaOR anteriorEsNumero)
                                                               (cond
                                                                 [(null? clausulaOR) (if (equal? anteriorEsNumero #t) #t #f)]
                                                                 [(equal? anteriorEsNumero #f) (if (not (is-var? (car clausulaOR))) #f (recorrerOR (cdr clausulaOR) #t)) ]
                                                                 [else (if (equal? (car clausulaOR) 'or) (recorrerOR (cdr clausulaOR) #f) #f)]
                                                                 )
                                                               )
                                                               ])
                                                 (if (equal? (list? clausulaOR) #f) #f (recorrerOR clausulaOR #f))
                                                )
                       ))

(define fnc->clausulaOR (lambda (FNC) (
                                          cond
                                            [(equal? (car FNC) 'FNC) (fnc->clausulaOR (cdr FNC))]
                                            [(number? (car FNC)) (fnc->clausulaOR (cdr FNC))]
                                            [else (car (car FNC))])
                                          ))
;;clausulaAND

(define clausulaAND (lambda (listOfClausulas) 
                                               (cond
                                               [(null? listOfClausulas) '()]
                                               [(null? (cdr listOfClausulas)) (list (clausulaOR (car listOfClausulas)))]
                                               [else (cons (clausulaOR (car listOfClausulas)) (cons 'and (clausulaAND (cdr listOfClausulas))))]
                                               )))

(define is-clausulaAND? (lambda (clausulaAND) (letrec (
                                                       [recorrerAND (lambda (clausulaAND anteriorEsLista)
                                                               (cond
                                                                 [(null? clausulaAND) (if (equal? anteriorEsLista #t) #t #f)]
                                                                 [(equal? anteriorEsLista #f) (if (not (is-clausulaOR? (car clausulaAND))) #f (recorrerAND (cdr clausulaAND) #t)) ]
                                                                 [else (if (equal? (car clausulaAND) 'and) (recorrerAND (cdr clausulaAND) #f) #f)]
                                                                 )
                                                               )
                                                               ])
                                                (if (equal? (list? clausulaAND) #f) #f (recorrerAND clausulaAND #f))
                                                )))

(define fnc->clausulaAND (lambda (FNC)
                                          (cond
                                            [(equal? (car FNC) 'FNC) (fnc->clausulaAND (cdr FNC))]
                                            [(number? (car FNC)) (fnc->clausulaAND (cdr FNC))]
                                            [else FNC])
                                          ))


;;FNC

(define FNC (lambda (n listOfClausulas) 
                         (list 'FNC n (clausulaAND listOfClausulas))
                         ))

(define is-FNC? (lambda (FNC) 
                               (if (and (equal? (car FNC) 'FNC) (number? (cadr FNC)) (is-clausulaAND? (caddr FNC))) #t #f)
                               ))

;; 1.2
(define-datatype clausulaORR clausulaORR?
  (clausulaORR-s
   (n number?))
  (clausulaORR-rec
   (num number?)
   (rec clausulaORR?))
 )

(define-datatype clausulasANDD clausulasANDD?
  (clausulaANDD-s
   (cl clausulaORR?))
  (clausulasANDD-rec
   (s clausulaORR?)
   (rec clausulasANDD?))
 )

(define-datatype fnc fnc?
  (FNCC
   (n number?)
   (clausulasANDD clausulasANDD?))
 )


;; Ejemplos de la implementacion basada en datatypes donde se crean instancias SAT

(FNCC
 3
 (clausulasANDD-rec
  (clausulaORR-rec 1
                  (clausulaORR-rec -2
                                  (clausulaORR-s 3)))
  (clausulaANDD-s
   (clausulaORR-rec -1
                   (clausulaORR-s 2)))))


(FNCC
 4
 (clausulasANDD-rec
  (clausulaORR-rec 1
                  (clausulaORR-rec -2
                                  (clausulaORR-rec 3
                                                  (clausulaORR-s 4))))
  (clausulaANDD-s
   (clausulaORR-s 2))))


(FNCC
 3
 (clausulasANDD-rec
  (clausulaORR-rec 1
                  (clausulaORR-rec -2
                                  (clausulaORR-s 3)))
  (clausulasANDD-rec
   (clausulaORR-rec -1
                   (clausulaORR-s 2))
   (clausulaANDD-s
    (clausulaORR-s 3)))))



;;2.1

(define unir (lambda (L1 L2)
  (if (null? L1) L2
      (cons (car L1) (unir (cdr L1) L2)))))


(define UNPARSEBNF-OR (lambda (exp)
                                   (cond
                                     [(null? (cdr exp)) (list (car exp))]
                                     [else (unir (list (car exp) 'or) (UNPARSEBNF-OR (cdr exp)))]
                                     )))

(define UNPARSEBNF-AND (lambda (exp)
                                    (cond
                                      [(null? (cdr exp)) (list (UNPARSEBNF-OR (car exp)))]
                                      [else (unir (list (UNPARSEBNF-OR (car exp)) 'and) (UNPARSEBNF-AND (cdr exp)))]
                                      )))


(define UNPARSEBNF (lambda (exp)
                                (if (and (equal? (car exp) 'FNC) (number? (cadr exp)) (list? (caddr exp)))
                                    (list 'FNC (cadr exp) (UNPARSEBNF-AND (caddr exp)))
                                (eopl:error 'exp "no es una FNC valida"))))
;; Casos de prueba
(UNPARSEBNF '(FNC 3 ((1 -2 3) (-1 2))))
(UNPARSEBNF '(FNC 2 ((1 2) (-1 -2) (1 -2))))
(UNPARSEBNF '(FNC 4 ((1 2 3) (-1 -2) (2 -3 4) (-4 1))))