#lang racket

#|

RF05 SUFFLE
|#

(require "base_21056415_LagosRivera.rkt")
(require "tda_carta_21056415_LagosRivera.rkt")
(require "tda_deck_21056415_LagosRivera.rkt")

(provide shuffleDeck)

;Funciones Auxiliares

;Generador de numeros pseudoaleatorios exigido por el enunciado
;Dom: Xn (int)
;rec: entero (int)
;funcion dada
(define (randomPuro Xn)
  (modulo (+ (* Xn 1103515245) 12345) 2147483648))

;Elimina recursivamente un elemento en una posicion especifica de la lista
;Dom: lista x n entero
;Rec: lista
;recursion natural
(define (quitar-en-posicion lista n)
  (if (= n 0)
      (cdr lista)
      (cons (car lista) (quitar-en-posicion (cdr lista) (- n 1)))))

;Mezcla los elementos de una lista de cartas de forma recursiva utilizando el generador randomPuro
;Dom: lista  x semilla (int)
; Rec: lista
; recursion natural
(define (mezclar-lista lista semilla)
  (if (null? lista)
      '()
      (let ((nueva-semilla (randomPuro semilla)))
        (let ((pos (modulo nueva-semilla (length lista))))
          (cons (list-ref lista pos)
                (mezclar-lista (quitar-en-posicion lista pos) nueva-semilla))))))

;

;Baraja las cartas de un mazo aplicando una semilla para generar numeros aleatorios
; Dom: d (deck) X semilla (int+)
; Rec: deck o #f

(define (shuffleDeck d semilla)
  (if (deck? d)
      (list 'deck (mezclar-lista (deck-cartas d) semilla))
      #f))