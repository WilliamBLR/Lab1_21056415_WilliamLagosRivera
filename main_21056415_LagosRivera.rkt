#lang racket

#|

RNF8
Archivo central que consolida todos los TDA
|#

;ImportaciIn de la base y TDAs de dato
(require "base_21056415_LagosRivera.rkt")
(require "tda_ataque_21056415_LagosRivera.rkt")
(require "tda_accion_21056415_LagosRivera.rkt")
(require "tda_energia_21056415_LagosRivera.rkt")
(require "tda_carta_21056415_LagosRivera.rkt")
(require "tda_criatura_21056415_LagosRivera.rkt")
(require "tda_deck_21056415_LagosRivera.rkt")
(require "tda_shuffle_21056415_LagosRivera.rkt")
(require "tda_jugador_21056415_LagosRivera.rkt")
(require "tda_estadojuego_21056415_LagosRivera.rkt")

;Importación de lOgica de juego y utilidades
(require "tda_juego_21056415_LagosRivera.rkt")
(require "tda_print_21056415_LagosRivera.rkt")
(require "tda_jugadas_21056415_LagosRivera.rkt")
(require "tda_usaraccion_21056415_LagosRivera.rkt")

;Re-exportaciOn de funciones principales (RF04 al RF12)
(provide (all-from-out "base_21056415_LagosRivera.rkt")
         (all-from-out "tda_ataque_21056415_LagosRivera.rkt")
         (all-from-out "tda_carta_21056415_LagosRivera.rkt")
         (all-from-out "tda_deck_21056415_LagosRivera.rkt")
         (all-from-out "tda_shuffle_21056415_LagosRivera.rkt")
         (all-from-out "tda_juego_21056415_LagosRivera.rkt")
         (all-from-out "tda_jugador_21056415_LagosRivera.rkt")
         (all-from-out "tda_print_21056415_LagosRivera.rkt")
         (all-from-out "tda_jugadas_21056415_LagosRivera.rkt"))

#| 

Este archivo permite la ejecuciON del laboratorio
Para verificar el funcionamiento, ejecute el archivo de pruebas
pruebas_21056415_LagosRivera.rkt
|#