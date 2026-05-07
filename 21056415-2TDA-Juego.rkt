#lang racket

(require "21056415-2base.rkt")
(require "21056415-2TDA-Carta.rkt")
(require "21056415-2TDA-Deck.rkt")
(require "21056415-2TDA-Shuffle.rkt")
(require "21056415-2TDA-Jugador.rkt")

(provide initGame
         juego?
         juego-jugador1
         juego-jugador2
         juego-turno
         set-juego-jugador1
         set-juego-jugador2
         set-juego-turno)

;funcion pseudoaleatoria obligatoria del enunciado
(define (randomPuro Xn)
  (modulo (+ (* Xn 1103515245) 12345) 2147483648))

;revisa si una lista de cartas tiene al menos un pokemon basico
(define (tiene-basico? mano)
  (if (null? mano)
      #f
      (if (and (carta-pokemon? (car mano))
               (null? (card-evoluciona-de (car mano))))
          #t
          (tiene-basico? (cdr mano)))))

;funcion recursiva que baraja y saca 7 cartas hasta que salga un basico
;retorna un par (mano . mazo-restante)
(define (obtener-mano-valida mazo-actual semilla)
  (let* ((mazo-barajado (shuffleDeck mazo-actual semilla))
         (cartas (deck-cartas mazo-barajado))
         (posible-mano (take cartas 7))
         (mazo-restante (drop cartas 7)))
    (if (tiene-basico? posible-mano)
        (cons posible-mano (list 'deck mazo-restante)) ;<-- evitamos el constructor estricto
        (obtener-mano-valida mazo-barajado (randomPuro semilla)))))

;prepara a un jugador completo (mano y premios)
(define (preparar-jugador id mazo semilla)
  (let* ((mano-y-mazo (obtener-mano-valida mazo semilla))
         (mano-lista (car mano-y-mazo))
         (mazo-post-mano (cdr mano-y-mazo))
         (cartas-restantes (deck-cartas mazo-post-mano))
         (premios-lista (take cartas-restantes 6))
         (mazo-final (list 'deck (drop cartas-restantes 6)))) ;<-- evitamos el constructor estricto
    (make-jugador id mazo-final mano-lista '() '() '() premios-lista)))

;constructor principal
(define (initGame mazo1 mazo2 semilla)
  (if (and (deck? mazo1) (deck? mazo2) (integer? semilla) (> semilla 0))
      (let* ((semilla-j1 (randomPuro semilla))
             (semilla-j2 (randomPuro semilla-j1))
             (semilla-turno (randomPuro semilla-j2))
             (jugador1 (preparar-jugador "Jugador 1" mazo1 semilla-j1))
             (jugador2 (preparar-jugador "Jugador 2" mazo2 semilla-j2))
             (turno (if (= (modulo semilla-turno 2) 0) 1 2)))
        (list 'juego jugador1 jugador2 turno))
      #f))

;funcion de pertenencia
(define (juego? j)
  (and (list? j)
       (= (length j) 4)
       (equal? (car j) 'juego)
       (jugador? (cadr j))
       (jugador? (caddr j))
       (or (= (cadddr j) 1) (= (cadddr j) 2))))

;selectores
(define (juego-jugador1 j) (if (juego? j) (cadr j) #f))
(define (juego-jugador2 j) (if (juego? j) (caddr j) #f))
(define (juego-turno j) (if (juego? j) (cadddr j) #f))

;modificadores
(define (set-juego-jugador1 j nuevo-j1)
  (if (and (juego? j) (jugador? nuevo-j1))
      (list 'juego nuevo-j1 (juego-jugador2 j) (juego-turno j))
      #f))

(define (set-juego-jugador2 j nuevo-j2)
  (if (and (juego? j) (jugador? nuevo-j2))
      (list 'juego (juego-jugador1 j) nuevo-j2 (juego-turno j))
      #f))

(define (set-juego-turno j nuevo-turno)
  (if (and (juego? j) (or (= nuevo-turno 1) (= nuevo-turno 2)))
      (list 'juego (juego-jugador1 j) (juego-jugador2 j) nuevo-turno)
      #f))