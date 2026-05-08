#lang racket

(require "base_21056415_LagosRivera.rkt")
(require "tda_carta_21056415_LagosRivera.rkt")

(provide deck
         deck?
         deck-cartas)

;revisa si hay al menos un pokemon basico en la lista
;dom: lista
;rec: un booleano
;recursion natural;
(define (contiene-pokemon-basico? lista)
  (if (null? lista)
      #f
      (if (and (carta-pokemon? (car lista)) 
               (null? (card-evoluciona-de (car lista))))
          #t
          (contiene-pokemon-basico? (cdr lista)))))

;cuenta cuantas veces aparece un nombre en la lista
;dom: un nombre x lissta
;rec: entero
;recursion natural




(define (contar-nombre nombre lista)
  (if (null? lista)
      0
      (if (equal? nombre (card-nombre (car lista)))
          (+ 1 (contar-nombre nombre (cdr lista)))
          (contar-nombre nombre (cdr lista)))))

;valida la regla de las 4 copias
;dom: lista x original
;rec: un booleano
;recursion natural
(define (validar-copias? lista original)
  (if (null? lista)
      #t
      (if (carta-energy? (car lista))
          (validar-copias? (cdr lista) original)
          (if (<= (contar-nombre (card-nombre (car lista)) original) 4)
              (validar-copias? (cdr lista) original)
              #f))))

;constructor

;creo mi mazo de cartas, luego validamos que sean las 60 cartas, que cumpla las reglas
;dom: cartas
;rec: una lista que viene del tda deck o un booleano falso
;no aplica recursion en este caso

(define (deck . cartas)
  (if (and (= (length cartas) 60)
           (contiene-pokemon-basico? cartas)
           (validar-copias? cartas cartas))
      (list 'deck cartas)
      #f))

;funcion de pertenencia

;por llamarlo de alguna manera:  revisa si un mazo de carta "funciona" o si es "valido

(define (deck? d)
  (and (list? d)
       (= (length d) 2)
       (equal? (car d) 'deck)
       (list? (cadr d)))) ;modificacion para compatibilidad --- RECORDAR REVISAR DESPUES (MENSAJE PROPIO PARA RECORDARME) 

;selector

;extrae la lista de cartas que contiene mi mazo
;dom: mazo
;rec: lista o falso
(define (deck-cartas d)
  (if (deck? d) (cadr d) #f))