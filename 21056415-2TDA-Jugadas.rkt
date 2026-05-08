#lang racket

(require "21056415-2base.rkt")
(require "21056415-2TDA-Carta.rkt")
(require "21056415-2TDA-Deck.rkt")
(require "21056415-2TDA-Jugador.rkt")
(require "21056415-2TDA-Juego.rkt")

(provide playToBench
         changeActivePokemon
         drawCardFromDeck
         useEnergyCard
         evolvePokemon)

;--- funciones auxiliares compartidas ---

(define (quitar-carta carta lista)
  (if (null? lista)
      '()
      (if (equal? carta (car lista))
          (cdr lista)
          (cons (car lista) (quitar-carta carta (cdr lista))))))

(define (carta-existe? carta lista)
  (if (null? lista)
      #f
      (if (equal? carta (car lista))
          #t
          (carta-existe? carta (cdr lista)))))

(define (reemplazar-carta vieja nueva lista)
  (if (null? lista)
      '()
      (if (equal? vieja (car lista))
          (cons nueva (cdr lista))
          (cons (car lista) (reemplazar-carta vieja nueva (cdr lista))))))

;--- funcion rf08 ---
(define (playToBench juego carta)
  (if (and (juego? juego) (carta-pokemon? carta))
      (let ((turno (juego-turno juego)))
        (let ((jugador-actual (if (= turno 1) (juego-jugador1 juego) (juego-jugador2 juego))))
          (let ((mano (jugador-mano jugador-actual))
                (banca (jugador-banca jugador-actual)))
            (if (and (carta-existe? carta mano) (< (length banca) 5))
                (let ((nueva-mano (quitar-carta carta mano))
                      (nueva-banca (append banca (list carta))))
                  (let ((jugador-actualizado (set-jugador-banca 
                                              (set-jugador-mano jugador-actual nueva-mano) 
                                              nueva-banca)))
                    (if (= turno 1)
                        (set-juego-jugador1 juego jugador-actualizado)
                        (set-juego-jugador2 juego jugador-actualizado))))
                juego))))
      juego))

;--- funcion rf09 ---
(define (changeActivePokemon juego carta)
  (if (and (juego? juego) (carta-pokemon? carta))
      (let ((turno (juego-turno juego)))
        (let ((jugador-actual (if (= turno 1) (juego-jugador1 juego) (juego-jugador2 juego))))
          (let ((banca (jugador-banca jugador-actual))
                (activo-actual (jugador-activo jugador-actual)))
            (if (carta-existe? carta banca)
                (if (null? activo-actual)
                    (let ((nueva-banca (quitar-carta carta banca)))
                      (let ((jugador-actualizado (set-jugador-activo
                                                  (set-jugador-banca jugador-actual nueva-banca)
                                                  carta)))
                        (if (= turno 1)
                            (set-juego-jugador1 juego jugador-actualizado)
                            (set-juego-jugador2 juego jugador-actualizado))))
                    (let ((nueva-banca (append (quitar-carta carta banca) (list activo-actual))))
                      (let ((jugador-actualizado (set-jugador-activo
                                                  (set-jugador-banca jugador-actual nueva-banca)
                                                  carta)))
                        (if (= turno 1)
                            (set-juego-jugador1 juego jugador-actualizado)
                            (set-juego-jugador2 juego jugador-actualizado)))))
                juego))))
      juego))

;--- funcion rf10 ---
(define (drawCardFromDeck juego)
  (if (juego? juego)
      (let ((turno (juego-turno juego)))
        (let ((jugador-actual (if (= turno 1) (juego-jugador1 juego) (juego-jugador2 juego))))
          (let ((mazo-lista (deck-cartas (jugador-mazo jugador-actual)))
                (mano (jugador-mano jugador-actual)))
            (if (not (null? mazo-lista))
                (let ((carta-robada (car mazo-lista))
                      (nuevo-mazo-lista (cdr mazo-lista)))
                  (let ((nuevo-mazo (list 'deck nuevo-mazo-lista))
                        (nueva-mano (append mano (list carta-robada))))
                    (let ((jugador-actualizado (set-jugador-mano 
                                                (set-jugador-mazo jugador-actual nuevo-mazo) 
                                                nueva-mano)))
                      (if (= turno 1)
                          (set-juego-jugador1 juego jugador-actualizado)
                          (set-juego-jugador2 juego jugador-actualizado)))))
                juego))))
      juego))

;--- funcion rf11 ---
(define (useEnergyCard juego pokemon energia)
  (if (and (juego? juego) (carta-pokemon? pokemon) (carta-energy? energia))
      (let ((turno (juego-turno juego)))
        (let ((jugador-actual (if (= turno 1) (juego-jugador1 juego) (juego-jugador2 juego))))
          (let ((mano (jugador-mano jugador-actual))
                (activo (jugador-activo jugador-actual))
                (banca (jugador-banca jugador-actual)))
            (if (carta-existe? energia mano)
                (let ((nueva-mano (quitar-carta energia mano))
                      (pokemon-actualizado (add-pokemon-energia pokemon energia)))
                  (if (equal? pokemon activo)
                      (let ((jugador-actualizado (set-jugador-activo 
                                                  (set-jugador-mano jugador-actual nueva-mano) 
                                                  pokemon-actualizado)))
                        (if (= turno 1)
                            (set-juego-jugador1 juego jugador-actualizado)
                            (set-juego-jugador2 juego jugador-actualizado)))
                      (if (carta-existe? pokemon banca)
                          (let ((nueva-banca (reemplazar-carta pokemon pokemon-actualizado banca)))
                            (let ((jugador-actualizado (set-jugador-banca 
                                                        (set-jugador-mano jugador-actual nueva-mano) 
                                                        nueva-banca)))
                              (if (= turno 1)
                                  (set-juego-jugador1 juego jugador-actualizado)
                                  (set-juego-jugador2 juego jugador-actualizado))))
                          juego)))
                juego))))
      juego))

;--- funcion rf12 ---
(define (evolvePokemon juego pokemon evolucion)
  (if (and (juego? juego) (carta-pokemon? pokemon) (carta-pokemon? evolucion))
      (let ((turno (juego-turno juego)))
        (let ((jugador-actual (if (= turno 1) (juego-jugador1 juego) (juego-jugador2 juego))))
          (let ((mano (jugador-mano jugador-actual))
                (activo (jugador-activo jugador-actual))
                (banca (jugador-banca jugador-actual)))
            
            (if (and (carta-existe? evolucion mano)
                     (equal? (card-evoluciona-de evolucion) (card-nombre pokemon)))
                (let ((nueva-mano (quitar-carta evolucion mano))
                      (evolucion-actualizada (append (take evolucion 12) (drop pokemon 12))))
                  
                  (if (equal? pokemon activo)
                      (let ((jugador-sin-carta (set-jugador-mano jugador-actual nueva-mano)))
                        (let ((jugador-actualizado (set-jugador-activo jugador-sin-carta evolucion-actualizada)))
                          (if (= turno 1)
                              (set-juego-jugador1 juego jugador-actualizado)
                              (set-juego-jugador2 juego jugador-actualizado))))
                      
                      (if (carta-existe? pokemon banca)
                          (let ((nueva-banca (reemplazar-carta pokemon evolucion-actualizada banca))
                                (jugador-sin-carta (set-jugador-mano jugador-actual nueva-mano)))
                            (let ((jugador-actualizado (set-jugador-banca jugador-sin-carta nueva-banca)))
                              (if (= turno 1)
                                  (set-juego-jugador1 juego jugador-actualizado)
                                  (set-juego-jugador2 juego jugador-actualizado))))
                          juego)))
                juego))))
      juego))