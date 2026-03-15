#lang eopl

;;1.1


;;Gramática:
;;  <FNC> := FNC <númeroDeRacket> (<clausulasAND>)
;;
;;  <clausulaOR> := <var> or <clausulaOR>
;;             := <var>
;;
;;  <clausulasAND> := (<clausulaOR>) and <clausulasAND>
;;              := (<clausulaOR>) 
;;
;; <var> := <númeroDeRacket>


;;var
(define var (lambda (sym) 
                              (if (number? sym) sym '())
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



;;2.1

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
                                                  )))

                                    
                                 
                                 (cond
                                   [(equal? (car lista) 'FNC) (cons 'FNC (PARSEBNF (cdr lista)))]
                                   [(number? (car lista)) (cons (car lista) (PARSEBNF (cdr lista)))]
                                   [else (list (parseAND (car lista)))]
                                   ))
                                 ))


;;3.1 Generar todas las combinaciones
(define combinar (lambda (list)


                   (if (null? list) '(()) 
                   
                   (letrec (

                                     (crearCombinaciones (lambda (list)

                                       (if (null? list) '() (cons (cons #f ( car list)) (cons (cons #t (car list)) (crearCombinaciones (cdr list)))))                    
                                                           
                                                           )
                                                           )
                                         
                                    (resto (combinar (cdr list)))
                                    )

                    (crearCombinaciones resto))
                                  
                   
                   
                   )))


                          