#lang racket

(require "21056415-2base.rkt")
(require "21056415-2TDA-Ataque.rkt")
(require "21056415-2TDA-Carta.rkt")
(require "21056415-2TDA-Deck.rkt")
(require "21056415-2TDA-Shuffle.rkt")
(require "21056415-2TDA-Juego.rkt")
(require "21056415-2TDA-Jugador.rkt")
(require "21056415-2TDA-Print.rkt")
(require "21056415-2TDA-Jugadas.rkt")

;funcion auxiliar para generar una lista con n copias de una carta
(define (repetir-carta n c)
  (if (= n 0)
      '()
      (cons c (repetir-carta (- n 1) c))))

;definicion de cartas para las pruebas
(define accion-dummy (lambda (j a) j))
(define atk-dummy (attack '() "ataque" "descripcion" accion-dummy))
(define pika (card 'pokemon "Pikachu" null 60 'lightning 'fighting null 1 #f null (list atk-dummy)))
(define ener (card 'energy "Energia Electrica" 'lightning))
(define pocion (card 'trainer "Superpocion" "objeto" "curar" accion-dummy))

(displayln "--- pruebas rf04: deck ---")

(define lista-valida
  (append (repetir-carta 4 pika)
          (repetir-carta 4 pocion)
          (repetir-carta 52 ener)))

(define mazo1 (apply deck lista-valida))
(displayln (if (deck? mazo1) "mazo 1: valido" "mazo 1: error"))

(displayln "--- pruebas rf05: shuffle ---")

;probamos barajar el mazo 1 con una semilla cualquiera
(define mazo-barajado (shuffleDeck mazo1 12345))

(displayln (if (deck? mazo-barajado) "shuffle: mazo barajado con exito" "shuffle: error"))

;verificamos que el orden haya cambiado comparando la primera carta
(define primera-original (car (deck-cartas mazo1)))
(define primera-barajada (car (deck-cartas mazo-barajado)))

(displayln (if (not (equal? primera-original primera-barajada))
               "resultado: el orden de las cartas cambio"
               "resultado: el orden se mantuvo o mala suerte en el azar"))

(displayln "--- pruebas rf06: initGame ---")

;creamos un segundo mazo valido usando la misma lista del rf04 para simplificar
(define mazo2 (apply deck lista-valida))

;iniciamos el juego usando la semilla que muestra el profesor en su script
(define juego-inicial (initGame mazo1 mazo2 1531312))

(displayln (if (juego? juego-inicial) "initGame: juego creado con exito" "initGame: error"))

;extraemos el jugador 1 del juego para verificar que sus zonas se crearon bien
(define j1-prueba (juego-jugador1 juego-inicial))

(displayln (string-append "nombre j1: " (jugador-nombre j1-prueba)))
(displayln "cantidad de cartas en mano (debe ser 7):")
(displayln (length (jugador-mano j1-prueba)))
(displayln "cantidad de cartas en premios (debe ser 6):")
(displayln (length (jugador-premios j1-prueba)))
(displayln "cantidad de cartas restantes en mazo (debe ser 47):")
(displayln (length (deck-cartas (jugador-mazo j1-prueba))))

(displayln "turno inicial asignado al jugador:")
(displayln (juego-turno juego-inicial))

(displayln "--- pruebas rf07: printGame ---")

;generamos el string pidiendo ver la perspectiva del jugador de turno inicial
(define vista-jugador (printGame juego-inicial (juego-turno juego-inicial)))

;lo imprimimos en la consola usando displayln
(displayln vista-jugador)

(displayln "--- pruebas rf08: playToBench ---")



;identificamos de quien es el turno actual en el juego inicial
(define turno-actual (juego-turno juego-inicial))
(define jugador-en-turno (if (= turno-actual 1) (juego-jugador1 juego-inicial) (juego-jugador2 juego-inicial)))
(define mano-actual (jugador-mano jugador-en-turno))

;funcion auxiliar usando tu propio tda para mayor seguridad
(define (encontrar-pokemon mano)
  (if (null? mano)
      #f
      (if (carta-pokemon? (car mano)) ;usamos tu funcion del tda carta
          (car mano)
          (encontrar-pokemon (cdr mano)))))

;extraemos la carta pokemon disponible en la mano
(define carta-para-banca (encontrar-pokemon mano-actual))

;usamos el selector card-nombre en lugar de cadr para evitar errores de posicion
(displayln (string-append "jugador " (number->string turno-actual) " jugara en su banca a: " (card-nombre carta-para-banca)))

;ejecutamos la jugada rf08
(define juego-post-banca (playToBench juego-inicial carta-para-banca))

;imprimimos el tablero del jugador de turno para verificar que la carta ya no esta en su mano y paso a su banca
(displayln (printGame juego-post-banca turno-actual))