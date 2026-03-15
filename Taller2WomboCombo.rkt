#lang eopl

;; Integrantes:
;; Juan Esteban Arias Saldaña (2417915)
;; Valeria Zamudio Arevalo (2415210)

;;1.1


;;Gramática:
;;  <FNC> := FNC <número-de-racket> (<clausulasAND>)
;;
;;  <clausulaOR> := <var> or <clausulaOR>
;;               := <var
>
;;
;;  <clausulasAND> := (<clausulaOR>) and <clausulasAND>
;;                 := (<clausulaOR>) 
;;
;; <var> := <número-de-racket>


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


;; 3

;; buscarClausula :
;; Proposito:
;; N x L x N -> V: Recibe un numero num (indice buscado), una lista L1
;; de valores booleanos y un acumulador acum (indice actual).
;; Devuelve el elemento de L1 que se encuentra en la posicion num.
;; <numero> := <número-de-racket>
;; <lista>  := ()
;;          := (<valor-de-scheme> <lista>)

(define buscarClausula (lambda (num L1 acum)
                                          (if (eq? acum num) (car L1)
                                          (buscarClausula num (cdr L1) (+ 1 acum))
                                          )))

;; Pruebas buscarClausula
(buscarClausula 1 '(#t #f #t) 1)
;; > #t
(buscarClausula 2 '(#f #f #t) 1)
;; > #f
(buscarClausula 4 '(#t #f #t #f) 1)
;; > #f


;; reemplazarClausulas :
;; Proposito:
;; L x L -> L': Recibe una combinacion de valores booleanos comb y
;; una clausula L2 (lista de enteros con signo que representan literales).
;; Devuelve una lista donde cada literal positivo es reemplazado por su
;; valor booleano en comb, y cada literal negativo por la negacion de su valor.
;; <comb>   := (<booleano> ...)
;; <lista> := ()
;;            := (<número-de-racket> <lista>)

(define reemplazarClausulas (lambda (comb L2)
                                           (cond
                                             [(null? L2) '()]
                                             [(> (car L2) 0) (cons (buscarClausula (car L2) comb 1) (reemplazarClausulas comb (cdr L2)))]
                                             [(< (car L2) 0) (cons (not (buscarClausula (* -1 (car L2)) comb 1)) (reemplazarClausulas comb (cdr L2)))]
                                             )))

;; Pruebas reemplazarClausulas
(reemplazarClausulas '(#t #f #t) '(1 2 3))
;; > (#t #f #t) 
(reemplazarClausulas '(#t #f #t) '(-1 -2 -3))
;; > (#f #t #f)
(reemplazarClausulas '(#t #f #t) '(1 -2 3))
;; > (#t #t #t)



;; reemplazarFNC :
;; Proposito:
;; L x L -> L': Recibe una combinacion de valores booleanos comb y
;; una FNC (lista de clausulas, cada una lista de enteros con signo).
;; Devuelve la FNC con cada literal reemplazado por su valor booleano
;; segun la combinacion dada.
;; <comb> := (<booleano> ...)
;; <FNC>  := ()
;;        := (<clausula> <FNC>)

(define reemplazarFNC (lambda (comb L2)
                                     (if (null? L2)'()
                                     (cons (reemplazarClausulas comb (car L2)) (reemplazarFNC comb (cdr L2))))))

;; Pruebas reemplazarFNC
(reemplazarFNC '(#t #f) '((1 2) (-1 2)))
;; > ((#t #f) (#f #f))
(reemplazarFNC '(#f #f) '((1 2) (1 -2)))
;; > ((#f #f) (#f #t))
(reemplazarFNC '(#t #t) '((1 -2) (-1 2)))
;; > ((#t #f) (#f #t))



;; evaluarOR :
;; Proposito:
;; L -> B: Recibe una clausula clauOR (lista de booleanos anidada, resultado
;; de reemplazarClausulas) y evalua la disyuncion de sus elementos.
;; Devuelve #t si al menos un elemento es #t, #f si todos son #f o la lista es vacia.
;; <clauOR> := ()
;;          := (#t <clauOR>)
;;          := (#f <clauOR>)

(define evaluarOR (lambda (clauOR)
                                    (cond
                                      [(null? clauOR) #f]
                                      [(eq? (car clauOR) #t) #t]
                                      [else (evaluarOR (cdr clauOR))]
                                      )))

;; Pruebas evaluarOR
(evaluarOR '(#f #f #f))
;; > #f
(evaluarOR '(#f #t #f))
;; > #t
(evaluarOR '())
;; > #f



;; evaluarAND :
;; Proposito:
;; L -> B: Recibe una FNC evaluada clauAND (lista de clausulas booleanas
;; anidadas, resultado de reemplazarFNC) y evalua la conjuncion de todas
;; las clausulas usando evaluarOR.
;; Devuelve #t si todas las clausulas son satisfechas, #f si alguna no lo es.
;; <clauAND> := ()
;;           := (<clauOR> <clauAND>)

(define evaluarAND (lambda (clauAND)
                                      (cond
                                        [(null? clauAND) #t]
                                        [(not (evaluarOR (car clauAND))) #f]
                                        [else (evaluarAND (cdr clauAND))]
                                        )))
;; Pruebas evaluarAND
(evaluarAND '((#t #f) (#f #t)))
;; > #t
(evaluarAND '((#f #f) (#f #t)))
;; > #f
(evaluarAND '())
;; > #t



;; probarCombinaciones :
;; Proposito:
;; L x L -> L|B: Recibe una FNC (lista de clausulas con literales enteros)
;; y listComb (lista de todas las combinaciones de valores booleanos posibles).
;; Prueba cada combinacion sobre la FNC usando evaluarAND.
;; Devuelve la primera combinacion que satisface la FNC, o #f si ninguna lo hace.
;; <FNC>      := lista de clausulas definidas al inicio del archivo
;; <listComb> := ()
;;            := (<comb> <listComb>)

(define probarCombinaciones (lambda (FNC listComb)
                                                  (cond
                                                    [(null? listComb) #f]
                                                    [(evaluarAND (reemplazarFNC (car listComb) FNC)) (car listComb)]
                                                    [else (probarCombinaciones FNC (cdr listComb))]
                                                    )))

;; Pruebas probarCombinaciones
(probarCombinaciones '((1 2)) '((#f #f)(#f #t)(#t #f)(#t #t)))
;; > (#t #f)
(probarCombinaciones '((1)(-1)) '((#f)(#t)))
;; > #f
(probarCombinaciones '((1 2 3)(1 -2 3)(-1 2 -3)) 
                     '((#f #f #f)(#f #f #t)(#f #t #f)(#f #t #t)
                       (#t #f #f)(#t #f #t)(#t #t #f)(#t #t #t)))
;; > (#f #f #t)



;; EVALUARSAT :
;; Proposito:
;; FNC -> L: Recibe una formula en FNC.
;; Verifica si la formula es una FNC valida mediante is-FNC?, luego obtiene
;; las variables y clausulas con PARSEBNF, genera todas las combinaciones
;; posibles de valores booleanos con combinaciones, y prueba cada una.
;; Devuelve (satisfactible <combinacion>) si existe asignacion que satisface
;; la FNC, o (insatisfactible ()) si no existe ninguna.
;; <FNC-input> := formula valida segun is-FNC? y PARSEBNF

;;(define EVALUARSAT (lambda (FNC)
                                ;;(if (is-FNC? FNC)
                                    ;;(if (probarCombinaciones (cdr (cdr (PARSEBNF FNC))) (combinaciones FNC))
                                        ;;(list 'satisfactible (probarCombinaciones (cdr (cdr (PARSEBNF FNC))) (combinaciones FNC)))
                                    ;;(list 'insatisfactible '()))
                                ;;(eopl:error 'EVALUARSAT "no es una FNC valida"))))

;;Pruebas EVALUARSAT
;;(EVALUARSAT '(FNC 4 ((1 or -3) and (2 or 4) and (-1 or -2 or 3) and (1 or 2 or -4))))

;;(EVALUARSAT '(FNC 3 ((1 or 2 or 3) and (-1 or 2) and (1 or -3))))

;;(EVALUARSAT '(FNC 2 ((1 or 2) and (-1) and (-2))))

