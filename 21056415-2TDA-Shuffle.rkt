#lang racket

(require "21056415-2base.rkt")
(require "21056415-2TDA-Carta.rkt")
(require "21056415-2TDA-Deck.rkt")

(provide shuffleDeck)

;--- funciones auxiliares ---

;elimina un elemento en una posicion especifica de la lista
(define (quitar-en-posicion lista n)
  (if (= n 0)
      (cdr lista)
      (cons (car lista) (quitar-en-posicion (cdr lista) (- n 1)))))

;mezcla los elementos de una lista de cartas de forma recursiva
(define (mezclar-lista lista)
  (if (null? lista)
      '()
      (let ((pos (random (length lista))))
        (cons (list-ref lista pos)
              (mezclar-lista (quitar-en-posicion lista pos))))))

;--- funcion principal rf05 ---

;descripcion: baraja las cartas de un mazo aplicando una semilla aleatoria
;dom: deck x entero
;rec: deck
(define (shuffleDeck d semilla)
  (if (deck? d)
      (begin
        (random-seed semilla);fija la semilla para el generador aleatorio
        (list 'deck (mezclar-lista (deck-cartas d))))
      #f))
