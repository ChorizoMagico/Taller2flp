#lang eopl

(provide (all-defined-out))

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

;; clausulaORR :
;; Proposito:
;; Define el tipo de dato clausulaORR que representa una clausula
;; disyuntiva (OR) como una lista de numeros enteros.
;; clausulaORR-s    : recibe un unico numero n (number?) y representa
;;                    una clausula con un solo elemento.
;; clausulaORR-rec  : recibe un numero num (number?) y una clausula
;;                    rec (clausulaORR?) y representa una clausula con
;;                    multiples numeros.
;; <clausulaORR> := (clausulaORR-s <number>)
;;               := (clausulaORR-rec <number> <clausulaORR>)

(define-datatype clausulaORR clausulaORR?
  (clausulaORR-s
   (n number?))
  (clausulaORR-rec
   (num number?)
   (rec clausulaORR?))
)

;; Pruebas clausulaORR
(clausulaORR-s 1)
(clausulaORR-rec 1 (clausulaORR-s 2))
(clausulaORR-rec -1 (clausulaORR-rec 2 (clausulaORR-s -3)))




;; clausulasANDD :
;; Proposito:
;; Define el tipo de dato clausulasANDD que representa una conjuncion
;; (AND) de clausulas disyuntivas (clausulaORR).
;; clausulaANDD-s   : recibe una unica clausula cl (clausulaORR?) y
;;                    representa una FNC con una sola clausula.
;; clausulasANDD-rec: recibe una clausula s (clausulaORR?) y una
;;                    conjuncion rec (clausulasANDD?) y representa
;;                    una FNC con multiples clausulas.
;; <clausulasANDD> := (clausulaANDD-s <clausulaORR>)
;;                := (clausulasANDD-rec <clausulaORR> <clausulasANDD>)

(define-datatype clausulasANDD clausulasANDD?
  (clausulaANDD-s
   (cl clausulaORR?))
  (clausulasANDD-rec
   (s clausulaORR?)
   (rec clausulasANDD?))
)



;; Pruebas clausulasANDD
(clausulaANDD-s (clausulaORR-s 1))
(clausulasANDD-rec (clausulaORR-rec 1 (clausulaORR-s 2))
                   (clausulaANDD-s (clausulaORR-s -1)))
(clausulasANDD-rec (clausulaORR-rec -1 (clausulaORR-s 2))
                   (clausulasANDD-rec (clausulaORR-rec 1 (clausulaORR-s -2))
                                      (clausulaANDD-s (clausulaORR-s 3))))

;; fnc :
;; Proposito:
;; Define el tipo de dato fnc que representa una Formula en
;; Forma Normal Conjuntiva (FNC) completa.
;; FNCC: recibe un numero n (number?) que indica la cantidad de
;;       variables y una conjuncion de clausulas clausulasANDD
;;       (clausulasANDD?) que representa la formula completa.
;; <fnc>          := (FNCC <number> <clausulasANDD>)
;; <clausulasANDD> := (clausulaANDD-s <clausulaORR>)
;;                := (clausulasANDD-rec <clausulaORR> <clausulasANDD>)
;; <clausulaORR>  := (clausulaORR-s <number>)
;;                := (clausulaORR-rec <number> <clausulaORR>)

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

