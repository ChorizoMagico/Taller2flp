#lang eopl

;;1.1


;;Gramática:
;;  <FNC> := FNC <númeroDeRacket> (<clausulas>)
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

(define is-clausulaOR? (lambda (clausulaOR)
                       (if (is-var? (car clausulaOR)) #t #f)
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
(define is-clausulaAND? (lambda (clausulaAND)
                       (and (list? clausulaAND) (or (list? (car clausulaAND)) (number? (car clausulaAND)))))
                       )

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
                               (if (equal? (car FNC) 'FNC) #t #f)
                               ))



