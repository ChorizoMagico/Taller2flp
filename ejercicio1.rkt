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
;;               := <var
>
;;
;;  <clausulasAND> := (<clausulaOR>) and <clausulasAND>
;;                 := (<clausulaOR>) 
;;
;; <var> := <número-de-racket>


;;var
;; var :
;; Proposito:
;; V -> V: Recibe un simbolo sym y verifica si es un numero.
;; Devuelve el numero si sym es un numero, o '() si no lo es.
;; <sym> := <número-de-racket>
;;       := <valor-de-scheme>
(define var (lambda (sym) 
                              (if (number? sym) sym '())
                              ))
;; Pruebas var
(var 1)
;; > 1
(var -2)
;; > -2
(var 'x)
;; > ()

;; is-var? :
;; Proposito:
;; V -> B: Recibe un simbolo sym y verifica si es un numero.
;; Devuelve #t si sym es un numero, #f en caso contrario.
;; <sym> := <número-de-racket>
;;       := <valor-de-scheme>
(define is-var? (lambda (sym) (if
                                  (number? sym) #t #f
                                  )))
;; Pruebas is-var?
(is-var? 1)
;; > #t
(is-var? -3)
;; > #t
(is-var? 'x)
;; > #f

;; fnc->var :
;; Proposito:
;; L x N -> B: Recibe una FNC (lista) y un numero var.
;; Devuelve #t si var aparece en alguna clausula de la FNC,
;; #f en caso contrario.
;; <FNC> := lista definida segun la gramatica al inicio del archivo
;; <var> := <número-de-racket>
(define fnc->var (lambda (FNC var) 
                                     (cond
                                       [(null? FNC) #f]
                                       [(list? (car FNC)) (or (fnc->var (car FNC) var) (fnc->var (cdr FNC) var))]
                                       [(and (is-var? (car FNC)) (equal? (car FNC) var)) #t]
                                       [else (fnc->var (cdr FNC) var)]
                                       )))

;; Pruebas fnc->var
(fnc->var '(FNC 3 (1 or -2) and (3)) 1)
;; > #t
(fnc->var '(FNC 3 (1 or -2) and (3)) -2)
;; > #t
(fnc->var '(FNC 3 (1 or -2) and (3)) 5)
;; > #f

;; clausulaOR :
;; Proposito:
;; L -> L': Recibe una lista de numeros enteros listVar y construye
;; una clausula disyuntiva con el simbolo 'or intercalado entre cada numero.
;; <listVar> := ()
;;           := (<número-de-racket> <listVar>)

(define clausulaOR (lambda (listVar)
                   (cond
                     [(null? listVar) '()]
                     [(null? (cdr listVar)) (list (var (car listVar)))]
                     [else (cons (var (car listVar)) (cons 'or (clausulaOR (cdr listVar))))]
                     )
                   ))

;; Pruebas clausulaOR
(clausulaOR '(1 2 3))
;; > (1 or 2 or 3)
(clausulaOR '(-1 2))
;; > (-1 or 2)
(clausulaOR '(1))
;; > (1)

;; is-clausulaOR? :
;; Proposito:
;; L -> B: Recibe una lista clausulaOR y verifica si tiene la forma
;; correcta de una clausula disyuntiva (numeros intercalados con 'or).
;; Devuelve #t si es valida, #f en caso contrario.
;; <clausulaOR> := (<número-de-racket>)
;;              := (<número-de-racket> or <clausulaOR>)

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
;; Pruebas is-clausulaOR?
(is-clausulaOR? '(1 or -2 or 3))
;; > #t
(is-clausulaOR? '(1))
;; > #t
(is-clausulaOR? '(1 or))
;; > #f

;; fnc->clausulaOR :
;; Proposito:
;; L -> L': Recibe una FNC (lista) y extrae la primera clausula OR.
;; Devuelve la primera clausula disyuntiva encontrada.
;; <FNC> := lista definida segun la gramatica al inicio del archivo
(define fnc->clausulaOR (lambda (FNC) (
                                          cond
                                            [(equal? (car FNC) 'FNC) (fnc->clausulaOR (cdr FNC))]
                                            [(number? (car FNC)) (fnc->clausulaOR (cdr FNC))]
                                            [else (car (car FNC))])
                                          ))
;; Pruebas fnc->clausulaOR
(fnc->clausulaOR '(FNC 2 ((1 or -2) and (3))))
;; > (1 or -2)
(fnc->clausulaOR '(FNC 3 ((1) and (2 or 3))))
;; > (1)
(fnc->clausulaOR '(FNC 1 ((1))))
;; > (1)

;; clausulaAND :
;; Proposito:
;; L -> L': Recibe una lista de clausulas listOfClausulas (cada una lista
;; de numeros enteros) y construye una conjuncion con el simbolo 'and
;; intercalado entre cada clausula OR.
;; <listOfClausulas> := ()
;;                   := (<clausulaOR> <listOfClausulas>)

(define clausulaAND (lambda (listOfClausulas) 
                                               (cond
                                               [(null? listOfClausulas) '()]
                                               [(null? (cdr listOfClausulas)) (list (clausulaOR (car listOfClausulas)))]
                                               [else (cons (clausulaOR (car listOfClausulas)) (cons 'and (clausulaAND (cdr listOfClausulas))))]
                                               )))
;; Pruebas clausulaAND
(clausulaAND '((1 2)(3 4)))
;; > ((1 or 2) and (3 or 4))
(clausulaAND '((1 -2 3)))
;; > ((1 or -2 or 3))
(clausulaAND '((1)(2)(-1)))
;; > ((1) and (2) and (-1))

;; is-clausulaAND? :
;; Proposito:
;; L -> B: Recibe una lista clausulaAND y verifica si tiene la forma
;; correcta de una conjuncion de clausulas OR intercaladas con 'and.
;; Devuelve #t si es valida, #f en caso contrario.
;; <clausulaAND> := (<clausulaOR>)
;;               := (<clausulaOR> and <clausulaAND>)
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
;; Pruebas is-clausulaAND?
(is-clausulaAND? '((1 or 2) and (3 or 4)))
;; > #t
(is-clausulaAND? '((1)))
;; > #t
(is-clausulaAND? '((1 or 2) and))
;; > #f

;; fnc->clausulaAND :
;; Proposito:
;; L -> L': Recibe una FNC (lista) y extrae la lista de clausulas AND.
;; Devuelve la estructura de clausulas sin el encabezado FNC ni el numero
;; de variables.
;; <FNC> := lista definida segun la gramatica al inicio del archivo
(define fnc->clausulaAND (lambda (FNC)
                                          (cond
                                            [(equal? (car FNC) 'FNC) (fnc->clausulaAND (cdr FNC))]
                                            [(number? (car FNC)) (fnc->clausulaAND (cdr FNC))]
                                            [else FNC])
                                          ))
;; Pruebas fnc->clausulaAND
(fnc->clausulaAND '(FNC 2 ((1 or -2) and (3 or 4))))
;; > (((1 or -2) and (3 or 4)))
(fnc->clausulaAND '(FNC 1 ((1))))
;; > (((1)))
(fnc->clausulaAND '(FNC 3 ((1 or 2) and (-1) and (-2))))
;; > (((1 or 2) and (-1) and (-2)))

;; tomarNúmerosFNC :
;; Proposito:
;; L -> L': Recibe una FNC (lista) y extrae todos los numeros que aparecen
;; en sus clausulas, convirtiendo los negativos a su valor absoluto.
;; Devuelve una lista plana con todos los literales en valor absoluto.
;; <FNC> := lista definida segun la gramatica al inicio del archivo
(define tomarNúmerosFNC (lambda (FNC) (letrec (

                                               [valorAbsoluto (lambda (número)
                                                                (if (< número 0) (* número -1) número)
                                                                )]

                                               [ unir (lambda (L1 L2)
                                                        (if (null? L1) L2
                                                            (cons (car L1) (unir (cdr L1) L2))))]
                                               
                                               [tomarNúmeros (lambda (lista)

                                                               (cond
                                                                 [(null? lista) '()]
                                                                 [(list? (car lista)) (unir (tomarNúmeros (car lista)) (tomarNúmeros (cdr lista)))]
                                                                 [(number? (car lista)) (cons (valorAbsoluto (car lista)) (tomarNúmeros (cdr lista)))]
                                                                 [else (tomarNúmeros (cdr lista))]
                                                                 )

                                                               
                                                               )]
                                               )
                                   (tomarNúmeros (car (fnc->clausulaAND FNC))) ))
                                       
                                   
                                    )
;; Pruebas tomarNúmerosFNC
(tomarNúmerosFNC '(FNC 2 (1 or -2) and (-1 or 2)))
;; > (1 2 1 2)
(tomarNúmerosFNC '(FNC 3 (1 or -2 or 3) and (-1 or 2)))
;; > (1 2 3 1 2)
(tomarNúmerosFNC '(FNC 1 (1) and (-1)))
;; > (1 1)


;; quitarRepetidos :
;; Proposito:
;; L -> L': Recibe una lista y elimina los elementos repetidos.
;; Devuelve una lista con los elementos unicos de la lista original.
;; <lista> := ()
;;          := (<valor-de-scheme> <lista>)
(define quitarRepetidos (lambda (lista) 
                                         (letrec (
                                                  [recorrer (lambda (a lista)
                                                              (cond
                                                                [(null? lista) #t]
                                                                [(equal? a (car lista)) #f]
                                                                [else (recorrer a (cdr lista))]))]
                                                                )
                                                              

                          (cond
                            [(null? lista) '()]
                            [(recorrer (car lista) (cdr lista)) (cons (car lista) (quitarRepetidos (cdr lista)))]
                            [else (quitarRepetidos (cdr lista))]
                            ))
                                                  ))
;; Pruebas quitarRepetidos
(quitarRepetidos '(1 2 1 3 2))
;; > (1 2 3)
(quitarRepetidos '(1 1 1))
;; > (1)
(quitarRepetidos '())
;; > ()

;; fncTieneBuenN :
;; Proposito:
;; L -> B: Recibe una FNC (lista) y verifica que el numero de variables
;; declarado coincida con la cantidad de variables unicas en las clausulas,
;; y que estas sean consecutivas desde 1 hasta n.
;; Devuelve #t si el n es correcto, #f en caso contrario.
;; <FNC> := lista definida segun la gramatica al inicio del archivo
(define fncTieneBuenN (lambda (FNC)
                        (letrec
                            (
                             [valorAbsoluto (lambda (número)
                                                                (if (< número 0) (* número -1) número)
                                                                )]
                             [contador (lambda (lista acumulador)
                                       (if (null? lista) acumulador (contador (cdr lista) (+ acumulador 1)))
                                       )]
                             [contar (lambda (listaOriginal lista acumulador total)
                                       (cond
                                         [(null? lista) #f]
                                         [(> acumulador total) #t]
                                         [(= (valorAbsoluto (car lista)) acumulador) (contar listaOriginal listaOriginal (+ acumulador 1) total)]
                                         [else (contar listaOriginal (cdr lista) acumulador total)]
                                         )
                                       )]
                             [listaDeNúmeros (quitarRepetidos (tomarNúmerosFNC FNC))]
                             [totalDeNúmeros (contador listaDeNúmeros 0)]
                             )
                          (if (not (= (cadr FNC) totalDeNúmeros)) #f 
                          (if (contar listaDeNúmeros listaDeNúmeros 1 totalDeNúmeros) #t #f
                            ))

                        )))
;; Pruebas fncTieneBuenN
(fncTieneBuenN '(FNC 2 (1 or 2) and (-1 or -2)))
;; > #t
(fncTieneBuenN '(FNC 3 (1 or 3) and (2)))
;; > #f
(fncTieneBuenN '(FNC 2 (1 or 3) and (2)))
;; > #f

;; is-FNC? :
;; Proposito:
;; L -> B: Recibe una lista FNC y verifica si tiene la forma correcta
;; de una FNC valida (encabezado FNC, numero de variables, clausulas AND y n que cumple las especificaciones del fnc).
;; Devuelve #t si es valida, #f en caso contrario.
;; <FNC> := lista definida segun la gramatica al inicio del archivo

(define is-FNC? (lambda (FNC) 
                               (if (and (equal? (car FNC) 'FNC) (number? (cadr FNC)) (is-clausulaAND? (caddr FNC)) (fncTieneBuenN FNC)) #t #f)
                               ))

;; Pruebas is-FNC?
(is-FNC? '(FNC 2 (1 or 2) and (-1 or -2)))
;; > #t
(is-FNC? '(FNC 1 (1) and (-1)))
;; > #t
(is-FNC? '(2 (1 or 2)))
;; > #f

;; FNC :
;; Proposito:
;; N x L -> L': Recibe un numero n (cantidad de variables) y una lista
;; de clausulas listOfClausulas (cada una lista de numeros enteros).
;; Construye y devuelve la representacion abstracta de una FNC.
;; <n>               := <número-de-racket>
;; <listOfClausulas> := ()
;;                   := (<clausulaOR> <listOfClausulas>)

(define FNC (lambda (n listOfClausulas) 
                         (if (is-FNC? (list 'FNC n (clausulaAND listOfClausulas))) (list 'FNC n (clausulaAND listOfClausulas)) (eopl:error 'FNC "no es una FNC valida")
                         )))
;; Pruebas FNC
(FNC 2 '((1 2)(-1 -2)))
;; > (FNC 2 (1 or 2) and (-1 or -2))
(FNC 3 '((1 -2 3)(-1 2)))
;; > (FNC 3 (1 or -2 or 3) and (-1 or 2))
(FNC 1 '((1)(-1)))
;; > (FNC 1 (1) and (-1))
