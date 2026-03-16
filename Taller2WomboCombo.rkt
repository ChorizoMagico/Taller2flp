#lang eopl

;; Integrantes:
;; Juan Esteban Arias Saldaña (2417915)
;; Valeria Zamudio Arevalo (2415210)

;; Declaración de uso de IA:
;; Se utilizó IA para: entender los conceptos del taller (árbol abstracto, concreto, etc), organizar mejor la documentación y los casos de prueba,
;; entender errores del compilador
;; Se declara que todos los integrantes comprenden y pueden explicar
;; completamente cada solución implementada.
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
(fnc->var '(FNC 3 ((1 or -2) and (3))) 1)
;; > #t
(fnc->var '(FNC 3 ((1 or -2) and (3))) -2)
;; > #t
(fnc->var '(FNC 3 ((1 or -2) and (3))) 5)
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
(tomarNúmerosFNC '(FNC 2 ((1 or -2) and (-1 or 2))))
;; > (1 2 1 2)
(tomarNúmerosFNC '(FNC 3 ((1 or -2 or 3) and (-1 or 2))))
;; > (1 2 3 1 2)
(tomarNúmerosFNC '(FNC 1 ((1)) and (-1)))
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
(fncTieneBuenN '(FNC 2 ((1 or 2) and (-1 or -2))))
;; > #t
(fncTieneBuenN '(FNC 3 ((1 or 3) and (2))))
;; > #f
(fncTieneBuenN '(FNC 2 ((1 or 3) and (2))))
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
(is-FNC? '(FNC 2 ((1 or 2) and (-1 or -2))))
;; > #t
(is-FNC? '(FNC 1 ((1) and (-1))))
;; > #t
(is-FNC? '(2 ((1 or 2))))
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


;; 3

;; combinar :
;; Proposito:
;; L -> L': Recibe una lista de elementos y genera todas las combinaciones
;; posibles de valores booleanos (#t y #f) para cada elemento.
;; Devuelve una lista de listas de booleanos.
;; <lista> := ()
;;          := (<valor-de-scheme> <lista>)
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
(combinaciones '(FNC 2 (1 or -2) and (-1 or 2)))
;; > ((#f #f) (#f #t) (#t #f) (#t #t))
(combinaciones '(FNC 1 (1) and (-1)))
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

(EVALUARSAT '(FNC 3 ((1 or 2 or 3) and (-1 or 2) and (1 or -3))))

(EVALUARSAT '(FNC 2 ((1 or 2) and (-1) and (-2))))

