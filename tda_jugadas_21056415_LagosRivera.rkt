#lang racket

(require "base_21056415_LagosRivera.rkt")
(require "tda_carta_21056415_LagosRivera.rkt")
(require "tda_deck_21056415_LagosRivera.rkt")
(require "tda_jugador_21056415_LagosRivera.rkt")
(require "tda_juego_21056415_LagosRivera.rkt")

;RF08 a RF12: Intento de manejar las acciones de los jugadores

(provide playToBench
         changeActivePokemon
         drawCardFromDeck
         useEnergyCard
         evolvePokemon)

;aux
;Funcion que busca una carta y me devuelve la carta junto a su lista
;dom: carta x lista
;rec lista
;recursion natural
(define (quitar-carta carta lista)
  (if (null? lista)
      '()
      (if (equal? carta (car lista))
          (cdr lista)
          (cons (car lista) (quitar-carta carta (cdr lista))))))

;funcion donde selecciono una carta y verifico si se encuentra dentro de una lista
;dom:carta x lista
;rec: un boleano
;recursion natural

(define (carta-existe? carta lista)
  (if (null? lista)
      #f
      (if (equal? carta (car lista))
          #t
          (carta-existe? carta (cdr lista)))))


;funcion para reemplazar una carta vieja por una nueva
;dom: carta antigua x carta nueva x lista
;rec: lista
;recursion natural


(define (reemplazar-carta vieja nueva lista)
  (if (null? lista)
      '()
      (if (equal? vieja (car lista))
          (cons nueva (cdr lista))
          (cons (car lista) (reemplazar-carta vieja nueva (cdr lista))))))

;RF08

;Mueve un pokemon desde el jugador actual hacia su banca
;dom: juego x carta
;rec: juego

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

;RF09

;cambia el pokemon actual por uno que este en banca
;dom; juego x carta
;rec: juego o en caso de que la jugada sea invalida devuelve el mismo jueg

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

;RF10
;toma la primera carta del mazo del jugador selec y la añade a su baraja
;dom: juego
;rec: juego


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

;funcion rf11

;carta de energia al pokemon que se indique


(define (useEnergyCard juego pokemon energia)

  
  (if (and (juego? juego) (carta-pokemon? pokemon) (carta-energy? energia))
      (let ((turno (juego-turno juego)))
        (let ((jugador-actual (if (= turno 1) (juego-jugador1 juego) (juego-jugador2 juego))))
          (let ((mano (jugador-mano jugador-actual))
                (activo (jugador-activo jugador-actual))

                
                (banca (jugador-banca jugador-actual)))
            (if (carta-existe? energia mano)

;borrador de prueba
                
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

;funcion rf12

;evolucioan a un pokemon que este en juego, transfiriendo sus ddaño y energias
;dom: juego x pokemon x evolucion
;rec juego


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