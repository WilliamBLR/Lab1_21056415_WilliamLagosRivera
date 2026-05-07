#lang racket

(require "21056415-2base.rkt")
(require "21056415-2TDA-Carta.rkt")

(provide deck
         deck?
         deck-cartas)

;revisa si hay al menos un pokemon basico en la lista
(define (contiene-pokemon-basico? lista)
  (if (null? lista)
      #f
      (if (and (carta-pokemon? (car lista)) 
               (null? (card-evoluciona-de (car lista))))
          #t
          (contiene-pokemon-basico? (cdr lista)))))

;cuenta cuantas veces aparece un nombre en la lista
(define (contar-nombre nombre lista)
  (if (null? lista)
      0
      (if (equal? nombre (card-nombre (car lista)))
          (+ 1 (contar-nombre nombre (cdr lista)))
          (contar-nombre nombre (cdr lista)))))

;valida la regla de las 4 copias (ignora energias)
(define (validar-copias? lista original)
  (if (null? lista)
      #t
      (if (carta-energy? (car lista))
          (validar-copias? (cdr lista) original)
          (if (<= (contar-nombre (card-nombre (car lista)) original) 4)
              (validar-copias? (cdr lista) original)
              #f))))

;constructor
(define (deck . cartas)
  (if (and (= (length cartas) 60)
           (contiene-pokemon-basico? cartas)
           (validar-copias? cartas cartas))
      (list 'deck cartas)
      #f))

;funcion de pertenencia
(define (deck? d)
  (and (list? d)
       (= (length d) 2)
       (equal? (car d) 'deck)
       (list? (cadr d)))) ;<-- aqui quitamos la exigencia de las 60 cartas

;selector
(define (deck-cartas d)
  (if (deck? d) (cadr d) #f))