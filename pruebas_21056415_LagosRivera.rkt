#lang racket

#|

me piden 3 casos de prueba exitosos y de errores



|#

(require "main_21056415_LagosRivera.rkt") ;llamo a todo


(define accion-sin-efecto (lambda (juego . args) juego))
(define accion-dormir (lambda (juego . args) juego))
(define accion-paralizar (lambda (juego . args) juego))

; --- 2. CONFIGURACIÓN DE CARTAS BASE ---
(define atk-dummy (attack '() "Ataque Base" "Sin efectos" accion-sin-efecto))

; Pokemones basicos, osea pokemones iniciales
(define pika (card 'pokemon "Pikachu" null 60 'lightning 'fighting null 1 #f null (list atk-dummy)))
(define squirtle (card 'pokemon "Squirtle" null 50 'water 'lightning null 1 #f null (list atk-dummy)))
(define charmander (card 'pokemon "Charmander" null 50 'fire 'water null 1 #f null (list atk-dummy)))

;Evoluciones correspondientes
(define raichu (card 'pokemon "Raichu" "Pikachu" 100 'lightning 'fighting null 1 #f null (list atk-dummy)))
(define wartortle (card 'pokemon "Wartortle" "Squirtle" 80 'water 'lightning null 1 #f null (list atk-dummy)))
(define charmeleon (card 'pokemon "Charmeleon" "Charmander" 80 'fire 'water null 1 #f null (list atk-dummy)))

;Energias y el entrenador
(define ener-elec (card 'energy "Energia Electrica" 'lightning))
(define ener-agua (card 'energy "Energia de Agua" 'water))
(define ener-fuego (card 'energy "Energia de Fuego" 'fire))
(define pocion (card 'trainer "Superpocion" "objeto" "curar" accion-sin-efecto))

; --- 3. PRUEBAS RF04: TDA Deck ---
(displayln "======= PRUEBAS RF04: deck ==========")
(define (repetir-carta n c) (if (= n 0) '() (cons c (repetir-carta (- n 1) c))))
(define lista-base (append (repetir-carta 4 pika) (repetir-carta 4 pocion) (repetir-carta 52 ener-elec)))

; 3 Ejemplos exitosos
(displayln "-------------Casos validos para el RF04: deck--------------")
(define mazo1 (apply deck lista-base))
(define mazo2 (apply deck (append (repetir-carta 4 squirtle) (repetir-carta 56 ener-agua))))
(define mazo3 (apply deck (append (repetir-carta 4 charmander) (repetir-carta 56 ener-fuego))))
(displayln (if (and (deck? mazo1) (deck? mazo2) (deck? mazo3)) "3 Mazos creados con exito." "Falla"))

; Caso de Error: Mazo con mas de 4 copias de una carta que no es energia (5 pociones)
(displayln "-------------Casos invalidos para el RF04: deck--------------")

(define mazo-error (apply deck (append (repetir-carta 4 pika) (repetir-carta 5 pocion) (repetir-carta 51 ener-elec))))
(displayln (if (not mazo-error) "Error: Mazo invalido rechazado (Regla de 4 copias)." "Falla"))


; --- 4. PRUEBAS RF05: shuffleDeck ---
(displayln "\n=== PRUEBAS RF05: shuffleDeck ===")
; 3 Ejemplos con semillas distintas
(define mazo-sh1 (shuffleDeck mazo1 100))
(define mazo-sh2 (shuffleDeck mazo1 200))
(define mazo-sh3 (shuffleDeck mazo1 300))
(displayln (if (and (deck? mazo-sh1) (deck? mazo-sh2) (deck? mazo-sh3)) "3 Mezclas realizadas con distintas semillas." "Falla"))


; --- 5. PRUEBAS RF06: initGame ---
(displayln "\n=== PRUEBAS RF06: initGame ===")
; 3 Ejemplos con semillas distintas
(define juego-init1 (initGame mazo1 mazo2 123))
(define juego-init2 (initGame mazo2 mazo3 456))
(define juego-init3 (initGame mazo3 mazo1 789))
(displayln (if (and (juego? juego-init1) (juego? juego-init2) (juego? juego-init3)) "3 Juegos inicializados." "Falla"))


; 
; PREPARACIÓN PARA LAS JUGADAS (RF08 a RF12)
; Para garantizar que los 3 ejemplos se puedan ejecutar sin fallar por azar, 
; colocaremos las cartas necesarias directamente en la mano del jugador actual
; 
(define turno-act (juego-turno juego-init1))
(define j-actual (if (= turno-act 1) (juego-jugador1 juego-init1) (juego-jugador2 juego-init1)))
(define mano-trucada (list pika squirtle charmander raichu wartortle charmeleon ener-elec ener-agua ener-fuego pocion))
(define j-trucado (set-jugador-mano j-actual mano-trucada))
(define juego-base (if (= turno-act 1) (set-juego-jugador1 juego-init1 j-trucado) (set-juego-jugador2 juego-init1 j-trucado)))


; --- 6. PRUEBAS RF08: playToBench ---
(displayln "\n=== PRUEBAS RF08: playToBench ===")
; 3 ejemplos exitosos
(displayln "--------- Casos validos para el RF08 ----------")

(define j-b1 (playToBench juego-base pika))
(displayln "RF08 Ejemplo 1: Pikachu movido a la banca.")
(define j-b2 (playToBench j-b1 squirtle))
(displayln "RF08 Ejemplo 2: Squirtle movido a la banca.")
(define j-b3 (playToBench j-b2 charmander))
(displayln "RF08 Ejemplo 3: Charmander movido a la banca.")

; Caso de Error: Intentar jugar una energia a la banca
(displayln "--------- Casos invalidos para el RF08 ----------")

(define j-b-err (playToBench j-b3 ener-elec))
(displayln (if (equal? j-b3 j-b-err) "No se puede jugar energia en la banca." "Falla"))


; --- 7. PRUEBAS RF09: changeActivePokemon ---
(displayln "\n=== PRUEBAS RF09: changeActivePokemon ===")
; 3 Ejemplos exitosos
(displayln "--------- Casos validos para el RF09 ----------")

(define j-a1 (changeActivePokemon j-b3 pika))
(displayln "RF09 Ejemplo 1: Pikachu pasa a ser el Activo.")
(define j-a2 (changeActivePokemon j-a1 squirtle))
(displayln "RF09 Ejemplo 2: Squirtle reemplaza a Pikachu como Activo.")
(define j-a3 (changeActivePokemon j-a2 charmander))
(displayln "RF09 Ejemplo 3: Charmander reemplaza a Squirtle como Activo.")


; --- 8. PRUEBAS RF10: drawCardFromDeck ---
(displayln "\n=== PRUEBAS RF10: drawCardFromDeck ===")
; 3 Ejemplos exitosos (Robar 3 veces seguidas)
(displayln "--------- Casos validos para el RF10 ----------")

(define j-d1 (drawCardFromDeck j-a3))
(displayln "RF10 Ejemplo 1: Se robo 1 carta.")
(define j-d2 (drawCardFromDeck j-d1))
(displayln "RF10 Ejemplo 2: Se robo una 2da carta.")
(define j-d3 (drawCardFromDeck j-d2))
(displayln "RF10 Ejemplo 3: Se robo una 3ra carta.")


; --- 9. PRUEBAS RF11: useEnergyCard ---
(displayln "\n=== PRUEBAS RF11: useEnergyCard ===")
; 3 Ejemplos exitosos
(displayln "--------- Casos validos para el RF11 ----------")

(define j-e1 (useEnergyCard j-d3 charmander ener-fuego))
(displayln "Ejemplo 1: Energia de fuego asignada a Charmander (Activo).")
(define j-e2 (useEnergyCard j-e1 pika ener-elec))
(displayln "Ejemplo 2: Energia electrica asignada a Pikachu (Banca).")
(define j-e3 (useEnergyCard j-e2 squirtle ener-agua))
(displayln "Ejemplo 3: Energia de agua asignada a Squirtle (Banca).")
(displayln "--------- Casos invalidos para el RF11 ----------")

; Caso de Error: Usar una carta de entrenador como energia
(define j-e-err (useEnergyCard j-e3 charmander pocion))
(displayln (if (equal? j-e3 j-e-err) "Carta de entrenador rechazada como energia." "Falla"))


; --- 10. PRUEBAS RF12: evolvePokemon ---
(displayln "\n=== PRUEBAS RF12: evolvePokemon ===")
; 3 Ejemplos exitosos
(displayln "--------- Casos validos para el RF12 ----------")

;correccion prueba
(define jugador-estado-actual (if (= turno-act 1) (juego-jugador1 j-e3) (juego-jugador2 j-e3)))
(define charmander-con-energia (jugador-activo jugador-estado-actual)) ; Charmander quedó como activo
(define pika-con-energia (car (jugador-banca jugador-estado-actual)))  ; Pikachu quedó 1ro en la banca
(define squirtle-con-energia (cadr (jugador-banca jugador-estado-actual))) ; Squirtle quedó 2do en la banca

(define j-ev1 (evolvePokemon j-e3 charmander-con-energia charmeleon))
(displayln "RF12 Ejemplo 1: Charmander (Activo) evoluciono a Charmeleon.")
(define j-ev2 (evolvePokemon j-ev1 pika-con-energia raichu))
(displayln "RF12 Ejemplo 2: Pikachu (Banca) evoluciono a Raichu.")
(define j-ev3 (evolvePokemon j-ev2 squirtle-con-energia wartortle))
(displayln "RF12 Ejemplo 3: Squirtle (Banca) evoluciono a Wartortle.")
(displayln "--------- Casos invalidos para el RF12 ----------")




;Eliminar al final, hay un error que debo corregir al final, lo usare como borrador de momento
#|
(define j-ev1 (evolvePokemon j-e3 charmander charmeleon))
(displayln "RF12 Ejemplo 1: Charmander (Activo) evoluciono a Charmeleon.")
(define j-ev2 (evolvePokemon j-ev1 pika raichu))
(displayln "RF12 Ejemplo 2: Pikachu (Banca) evoluciono a Raichu.")
(define j-ev3 (evolvePokemon j-ev2 squirtle wartortle))
(displayln "RF12 Ejemplo 3: Squirtle (Banca) evoluciono a Wartortle.")
(displayln "--------- Casos invalidos para el RF12 ----------")

|#

; Caso de Error: Intentar evolucionar a Charmeleon usando a Raichu
(define j-ev-err (evolvePokemon j-ev3 charmeleon raichu))
(displayln (if (equal? j-ev3 j-ev-err) "Evolucion incompatible rechazada." "Falla"))


; --- 11. PRUEBAS RF07: printGame ---
(displayln "\n=== PRUEBAS RF07: printGame (3 Impresiones) ===")
(displayln "\n--- RF07 Ejemplo 1 (Estado despues de mover a la banca) ---")
(displayln (printGame j-b3 turno-act))

(displayln "\n--- RF07 Ejemplo 2 (Estado despues de poner energias) ---")
(displayln (printGame j-e3 turno-act))

(displayln "\n--- RF07 Ejemplo 3 (Estado final tras las 3 evoluciones) ---")
(displayln (printGame j-ev3 turno-act))