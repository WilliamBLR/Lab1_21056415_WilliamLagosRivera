#lang racket

(require "21056415-2base.rkt")
(require "21056415-2TDA-Carta.rkt")
(require "21056415-2TDA-Deck.rkt")
(require "21056415-2TDA-Jugador.rkt")
(require "21056415-2TDA-Juego.rkt")

(provide playToBench
         changeActivePokemon
         drawCardFromDeck)

;--- funciones auxiliares compartidas ---

;elimina la primera ocurrencia de una carta especifica en una lista
(define (quitar-carta carta lista)
  (if (null? lista)
      '()
      (if (equal? carta (car lista))
          (cdr lista)
          (cons (car lista) (quitar-carta carta (cdr lista))))))

;verifica si una carta realmente existe dentro de una lista
(define (carta-existe? carta lista)
  (if (null? lista)
      #f
      (if (equal? carta (car lista))
          #t
          (carta-existe? carta (cdr lista)))))

;--- funcion rf08 ---

;descripcion: mueve un pokemon de la mano a la banca del jugador actual si hay espacio
;dom: juego x carta
;rec: juego
(define (playToBench juego carta)
  (if (and (juego? juego) (carta-pokemon? carta))
      (let* ((turno (juego-turno juego))
             (jugador-actual (if (= turno 1) (juego-jugador1 juego) (juego-jugador2 juego)))
             (mano (jugador-mano jugador-actual))
             (banca (jugador-banca jugador-actual)))
        
        (if (and (carta-existe? carta mano) (< (length banca) 5))
            (let* ((nueva-mano (quitar-carta carta mano))
                   (nueva-banca (append banca (list carta)))
                   (jugador-actualizado (set-jugador-banca 
                                         (set-jugador-mano jugador-actual nueva-mano) 
                                         nueva-banca)))
              
              (if (= turno 1)
                  (set-juego-jugador1 juego jugador-actualizado)
                  (set-juego-jugador2 juego jugador-actualizado)))
            juego))
      juego))

;--- funcion rf09 ---

;descripcion: mueve un pokemon de la banca al puesto activo (retirando al anterior a la banca si existe)
;dom: juego (Juego) X cartaDeLaBanca (card)
;rec: Juego
(define (changeActivePokemon juego carta)
  (if (and (juego? juego) (carta-pokemon? carta))
      (let* ((turno (juego-turno juego))
             (jugador-actual (if (= turno 1) (juego-jugador1 juego) (juego-jugador2 juego)))
             (banca (jugador-banca jugador-actual))
             (activo-actual (jugador-activo jugador-actual)))

        (if (carta-existe? carta banca)
            (if (null? activo-actual)
                ;caso 1: no hay pokemon activo
                (let* ((nueva-banca (quitar-carta carta banca))
                       (jugador-actualizado (set-jugador-activo
                                             (set-jugador-banca jugador-actual nueva-banca)
                                             carta)))
                  (if (= turno 1)
                      (set-juego-jugador1 juego jugador-actualizado)
                      (set-juego-jugador2 juego jugador-actualizado)))

                ;caso 2: hay pokemon activo (vuelve a la banca)
                (let* ((nueva-banca (append (quitar-carta carta banca) (list activo-actual)))
                       (jugador-actualizado (set-jugador-activo
                                             (set-jugador-banca jugador-actual nueva-banca)
                                             carta)))
                  (if (= turno 1)
                      (set-juego-jugador1 juego jugador-actualizado)
                      (set-juego-jugador2 juego jugador-actualizado))))
            juego))
      juego))

;--- funcion rf10 ---

;descripcion: roba la primera carta del mazo y la anade a la mano del jugador actual
;dom: juego (Juego)
;rec: Juego
(define (drawCardFromDeck juego)
  (if (juego? juego)
      (let* ((turno (juego-turno juego))
             (jugador-actual (if (= turno 1) (juego-jugador1 juego) (juego-jugador2 juego)))
             (mazo-lista (deck-cartas (jugador-mazo jugador-actual)))
             (mano (jugador-mano jugador-actual)))
        
        ;verificamos que el mazo no este vacio antes de robar
        (if (not (null? mazo-lista))
            (let* ((carta-robada (car mazo-lista))
                   (nuevo-mazo-lista (cdr mazo-lista))
                   ;reconstruimos el mazo saltando la validacion de 60 cartas
                   (nuevo-mazo (list 'deck nuevo-mazo-lista)) 
                   ;agregamos la carta al final de la mano
                   (nueva-mano (append mano (list carta-robada)))
                   ;actualizamos mazo y mano en el jugador
                   (jugador-actualizado (set-jugador-mano 
                                         (set-jugador-mazo jugador-actual nuevo-mazo) 
                                         nueva-mano)))
              
              ;devolvemos el juego con el jugador actualizado
              (if (= turno 1)
                  (set-juego-jugador1 juego jugador-actualizado)
                  (set-juego-jugador2 juego jugador-actualizado)))
            
            juego))
      juego))



