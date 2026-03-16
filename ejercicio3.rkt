#lang eopl


(require "ejercicio1.rkt")
(require "ejercicio2.rkt")

;; Integrantes:
;; Juan Esteban Arias Saldaña (2417915)
;; Valeria Zamudio Arevalo (2415210)

;; 3

;; combinar :
;; Proposito:
;; L -> L': Recibe una lista de elementos y genera todas las combinaciones
;; posibles de valores booleanos (#t y #f) para cada elemento.
;; Devuelve una lista de listas de booleanos.
;; <lista> := ()
;;          := (<valor-de-scheme> <lista>)

;; crearCombinaciones :
;; Proposito:
;; L -> L': Recibe una lista list de combinaciones parciales de booleanos
;; (cada elemento es una lista de booleanos).
;; Por cada combinacion existente genera dos nuevas: una anteponiendo #f
;; y otra anteponiendo #t.
;; Devuelve la lista con todas las combinaciones extendidas.
;; <list> := ()
;;         := (<combinacion-parcial> <list>)
;; <combinacion-parcial> := (<booleano> ...)

(define combinar (lambda (list)


                   (if (null? list) '(()) 
                   
                   (letrec (

                                     (crearCombinaciones (lambda (list)

                                       (if (null? list) '() (cons (cons #f ( car list)) (cons (cons #t (car list)) (crearCombinaciones (cdr list)))))                    
                                                           
                                                           )
                                                           )
                                         
                                    (resto (combinar (cdr list)))
                                    )

                    (crearCombinaciones resto)))))
;; Pruebas combinar
(combinar '(1 2))
;; > ((#f #f) (#f #t) (#t #f) (#t #t))
(combinar '(1))
;; > ((#f) (#t))
(combinar '())
;; > (())

;; Pruebas crearCombinaciones
;;(crearCombinaciones '((#f) (#t)))
;; > ((#f #f) (#t #f) (#f #t) (#t #t))
;;(crearCombinaciones '(()))
;; > ((#f) (#t))
;;(crearCombinaciones '())
;; > ()




;; combinaciones :
;; Proposito:
;; L -> L': Recibe una FNC (lista) y genera todas las combinaciones
;; posibles de valores booleanos para sus variables.
;; Devuelve una lista de listas de booleanos.
;; <FNC> := lista definida segun la gramatica al inicio del archivo
(define combinaciones (lambda (FNC)
                        (combinar (quitarRepetidos (tomarNúmerosFNC FNC)))
                        ))
;; Pruebas combinaciones
(combinaciones '(FNC 2 ((1 or -2) and (-1 or 2))))
;; > ((#f #f) (#f #t) (#t #f) (#t #t))
(combinaciones '(FNC 1 ((1) and (-1))))
;; > ((#f) (#t))
(combinaciones '(FNC 3 (1 or 2 or 3)))
;; > ((#f #f #f) (#f #f #t) (#f #t #f) (#f #t #t) (#t #f #f) (#t #f #t) (#t #t #f) (#t #t #t))

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

(define EVALUARSAT (lambda (FNC)
                                (if (is-FNC? FNC)
                                    (if (probarCombinaciones (car (cdr (cdr (PARSEBNF FNC)))) (combinaciones FNC))
                                        (list 'satisfactible (probarCombinaciones (car (cdr (cdr (PARSEBNF FNC)))) (combinaciones FNC)))
                                    (list 'insatisfactible '()))
                                (eopl:error 'EVALUARSAT "no es una FNC valida"))))

;;Pruebas EVALUARSAT
(EVALUARSAT '(FNC 4 ((1 or -3) and (2 or 4) and (-1 or -2 or 3) and (1 or 2 or -4))))
;; > (satisfactible (#f #t #f #f))
(EVALUARSAT '(FNC 3 ((1 or 2 or 3) and (-1 or 2) and (1 or -3))))
;; > (satisfactible (#f #t #f))
(EVALUARSAT '(FNC 2 ((1 or 2) and (-1) and (-2))))
;; > (insatisfactible ())
