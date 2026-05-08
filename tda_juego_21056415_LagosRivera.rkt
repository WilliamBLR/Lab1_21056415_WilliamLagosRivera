#lang racket

#|


RF06: Iniciar Juego

|#

(require "base_21056415_LagosRivera.rkt")
(require "tda_carta_21056415_LagosRivera.rkt")
(require "tda_deck_21056415_LagosRivera.rkt")
(require "tda_shuffle_21056415_LagosRivera.rkt")
(require "tda_jugador_21056415_LagosRivera.rkt")

(provide initGame
         juego?
         juego-jugador1
         juego-jugador2
         juego-turno
         set-juego-jugador1
         set-juego-jugador2
         set-juego-turno)

;Funciones Aux

; Descripción: Generador 
; Dom: Xn
; Rec: entero 
; Tipo recursión: No aplica
(define (randomPuro Xn)
  (modulo (+ (* Xn 1103515245) 12345) 2147483648))

;Revisa recursivamente si una lista de cartas tiene al menos un pokemon basico
;Dom: mano 
;Rec: booleano 
;recur: Natural
(define (tiene-basico? mano)
  (if (null? mano)
      #f
      (if (and (carta-pokemon? (car mano))
               (null? (card-evoluciona-de (car mano))))
          #t
          (tiene-basico? (cdr mano)))))

;Funcion recursiva que baraja y saca 7 cartas hasta que la mano incluya un basico
;Dom: mazo-actual x semilla (int)
;Rec: par (cons lista-mano deck-restante)
; recursionNatural
(define (obtener-mano-valida mazo-actual semilla)
  (let ((mazo-barajado (shuffleDeck mazo-actual semilla)))
    (let ((cartas (deck-cartas mazo-barajado)))
      (let ((posible-mano (take cartas 7))
            (mazo-restante (drop cartas 7)))
        (if (tiene-basico? posible-mano)
            (cons posible-mano (list 'deck mazo-restante)) 
            (obtener-mano-valida mazo-barajado (randomPuro semilla)))))))

;Construye un jugador repartiendo su mano inicial y premios desde un mazo.
; Dom: id (string) X mazo (deck) X semilla (int)
;Rec: lista (jugador)

(define (preparar-jugador id mazo semilla)
  (let ((mano-y-mazo (obtener-mano-valida mazo semilla)))
    (let ((mano-lista (car mano-y-mazo))
          (mazo-post-mano (cdr mano-y-mazo)))
      (let ((cartas-restantes (deck-cartas mazo-post-mano)))
        (let ((premios-lista (take cartas-restantes 6))
              (mazo-final (list 'deck (drop cartas-restantes 6)))) 
          (make-jugador id mazo-final mano-lista '() '() '() premios-lista))))))



;Inicia el juego barajando, entregando cartas a cada jugador y asignando el primer turno.
; Dom: mazo1 (deck) X mazo2 (deck) X semilla (int+)
; Rec: lista (TDA Juego) o #f

(define (initGame mazo1 mazo2 semilla)
  (if (and (deck? mazo1) (deck? mazo2) (integer? semilla) (> semilla 0))
      (let ((semilla-j1 (randomPuro semilla)))
        (let ((semilla-j2 (randomPuro semilla-j1)))
          (let ((semilla-turno (randomPuro semilla-j2)))
            (let ((jugador1 (preparar-jugador "Jugador 1" mazo1 semilla-j1))
                  (jugador2 (preparar-jugador "Jugador 2" mazo2 semilla-j2)))
              (let ((turno (if (= (modulo semilla-turno 2) 0) 1 2)))
                (list 'juego jugador1 jugador2 turno))))))
      #f))

;perten

;Verifica si una estructura cumple con el formato de un estado de Juego
;Rec: booleano

(define (juego? j)
  (and (list? j)
       (= (length j) 4)
       (equal? (car j) 'juego)
       (jugador? (cadr j))
       (jugador? (caddr j))
       (or (= (cadddr j) 1) (= (cadddr j) 2))))

;selectores


; Dom: juego
; Rec: lista jugado o #f
(define (juego-jugador1 j) (if (juego? j) (cadr j) #f))

;Obtiene el jugador 2 de la partida.
;Dom: juego
;Rec: lista o #f
(define (juego-jugador2 j) (if (juego? j) (caddr j) #f))

;Obtiene el numero del turno actual (1 o 2)

(define (juego-turno j) (if (juego? j) (cadddr j) #f))

; --- Modificadores ---

; Actualiza los datos del jugador 1 en el estado de juego
; Dom: juego x nuevo-j1 jugador
; Rec: lista juego o #f
(define (set-juego-jugador1 j nuevo-j1)
  (if (and (juego? j) (jugador? nuevo-j1))
      (list 'juego nuevo-j1 (juego-jugador2 j) (juego-turno j))
      #f))


(define (set-juego-jugador2 j nuevo-j2)
  (if (and (juego? j) (jugador? nuevo-j2))
      (list 'juego (juego-jugador1 j) nuevo-j2 (juego-turno j))
      #f))

;Cambia o actualiza el turno actual
; Dom: j (juego) X nuevo-turno (int)
; Rec: lista (juego) o #f
(define (set-juego-turno j nuevo-turno)
  (if (and (juego? j) (or (= nuevo-turno 1) (= nuevo-turno 2)))
      (list 'juego (juego-jugador1 j) (juego-jugador2 j) nuevo-turno)
      #f))