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

;funcion auxiliar para generar una lista con X copias de una carta
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

(displayln "pruebas rf04: deck")

(define lista-valida
  (append (repetir-carta 4 pika)
          (repetir-carta 4 pocion)
          (repetir-carta 52 ener)))

(define mazo1 (apply deck lista-valida))
(displayln (if (deck? mazo1) "mazo 1: valido" "mazo 1: error"))

(displayln "pruebas rf05: shuffle")

;probamos barajar el mazo 1 con una semilla cualquiera
(define mazo-barajado (shuffleDeck mazo1 12345))

(displayln (if (deck? mazo-barajado) "shuffle: mazo barajado con exito" "shuffle: error"))

;verificamos que el orden haya cambiado comparando la primera carta
(define primera-original (car (deck-cartas mazo1)))
(define primera-barajada (car (deck-cartas mazo-barajado)))

(displayln (if (not (equal? primera-original primera-barajada))
               "resultado: el orden de las cartas cambio"
               "resultado: el orden se mantuvo o mala suerte en el azar"))

(displayln "pruebas rf06")

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

(displayln "pruebas rf07: printGame")

;generamos el string pidiendo ver la perspectiva del jugador de turno inicial
(define vista-jugador (printGame juego-inicial (juego-turno juego-inicial)))

;lo imprimimos en la consola usando displayln
(displayln vista-jugador)

(displayln "pruebas rf08: playToBench")



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



(displayln "pruebas rf09: changeActivePokemon")

;identificamos al jugador en turno del estado anterior
(define turno-rf09 (juego-turno juego-post-banca))
(define jugador-rf09 (if (= turno-rf09 1) (juego-jugador1 juego-post-banca) (juego-jugador2 juego-post-banca)))

;extraemos a nuestro pokemon directamente desde la primera posicion de la banca
(define carta-para-activo (car (jugador-banca jugador-rf09)))

(displayln (string-append "jugador " (number->string turno-rf09) " movera al puesto activo a: " (card-nombre carta-para-activo)))

;ejecutamos la jugada rf09
(define juego-post-activo (changeActivePokemon juego-post-banca carta-para-activo))

;imprimimos el tablero para verificar que la banca quedo vacia y el pokemon esta activo
(displayln (printGame juego-post-activo turno-rf09))



(displayln "--- pruebas rf10: drawCardFromDeck ---")

;identificamos al jugador en turno del estado anterior
(define turno-rf10 (juego-turno juego-post-activo))
(define jugador-rf10 (if (= turno-rf10 1) (juego-jugador1 juego-post-activo) (juego-jugador2 juego-post-activo)))

(displayln (string-append "cartas en mano antes de robar: " (number->string (length (jugador-mano jugador-rf10)))))
(displayln (string-append "cartas en mazo antes de robar: " (number->string (length (deck-cartas (jugador-mazo jugador-rf10))))))

;ejecutamos la jugada rf10
(define juego-post-robo (drawCardFromDeck juego-post-activo))

;extraemos al jugador actualizado para comprobar los cambios
(define jugador-robado (if (= turno-rf10 1) (juego-jugador1 juego-post-robo) (juego-jugador2 juego-post-robo)))

(displayln "--- despues de usar drawCardFromDeck ---")
(displayln (string-append "cartas en mano ahora: " (number->string (length (jugador-mano jugador-robado)))))
(displayln (string-append "cartas en mazo ahora: " (number->string (length (deck-cartas (jugador-mazo jugador-robado))))))


(displayln "--- pruebas rf11: useEnergyCard ---")

;identificamos al jugador en turno del estado anterior
(define turno-rf11 (juego-turno juego-post-robo))
(define jugador-rf11 (if (= turno-rf11 1) (juego-jugador1 juego-post-robo) (juego-jugador2 juego-post-robo)))

;buscamos una energia en la mano para usar
(define (encontrar-energia mano)
  (if (null? mano)
      #f
      (if (carta-energy? (car mano))
          (car mano)
          (encontrar-energia (cdr mano)))))

(define carta-energia (encontrar-energia (jugador-mano jugador-rf11)))
;usaremos el pokemon que pusimos en la zona activa en el rf09
(define pokemon-objetivo (jugador-activo jugador-rf11))

(displayln (string-append "jugador " (number->string turno-rf11) " le pondra la " (card-nombre carta-energia) " a " (card-nombre pokemon-objetivo)))

;ejecutamos la jugada rf11
(define juego-post-energia (useEnergyCard juego-post-robo pokemon-objetivo carta-energia))

;imprimimos el tablero para verificar que el pokemon tiene la energia y la mano bajo en 1
(displayln (printGame juego-post-energia turno-rf11))



(displayln "--- pruebas rf12: evolvePokemon ---")

(displayln "--- pruebas rf12: evolvePokemon ---")

; 1. Creamos la carta de raichu para la prueba (evoluciona de Pikachu)
(define raichu-prueba (card 'pokemon "Raichu" "Pikachu" 100 'lightning 'fighting null 1 #f null (list atk-dummy)))

; 2. Identificamos al jugador actual desde el ÚLTIMO estado (juego-post-energia)
(define turno-rf12 (juego-turno juego-post-energia))
(define jugador-rf12 (if (= turno-rf12 1) (juego-jugador1 juego-post-energia) (juego-jugador2 juego-post-energia)))

; 3. EXTRAEMOS EL PIKACHU ACTUALIZADO (el que ya tiene la energía puesta)
(define pikachu-actualizado (jugador-activo jugador-rf12))

; 4. Le inyectamos el Raichu a la mano a la fuerza para garantizar la prueba
(define mano-con-raichu (cons raichu-prueba (jugador-mano jugador-rf12)))
(define jugador-trucado (let ((jugador-temporal (set-jugador-mano jugador-rf12 mano-con-raichu))) jugador-temporal))

(define juego-trucado (if (= turno-rf12 1) 
                          (set-juego-jugador1 juego-post-energia jugador-trucado)
                          (set-juego-jugador2 juego-post-energia jugador-trucado)))

(displayln (string-append "jugador " (number->string turno-rf12) " evolucionara a Pikachu en Raichu"))

; 5. Ejecutamos la jugada rf12 pasando el pikachu-actualizado
(define juego-post-evolucion (evolvePokemon juego-trucado pikachu-actualizado raichu-prueba))

; 6. Imprimimos el tablero final
(displayln (printGame juego-post-evolucion turno-rf12))