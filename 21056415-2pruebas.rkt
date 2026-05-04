#lang racket


(require "21056415-2TDA-Jugador.rkt")
(require "21056415-2TDA-Estado-Juego.rkt")


(define j1 (make-jugador 'ash '() '() '() '() '() '()))
(define j2 (make-jugador 'misty '() '() '() '() '() '()))

(define partida
  (make-estado-juego j1 j2 1 '()))

partida
(estado-juego? partida)
(estado-juego-turno partida)
(set-estado-turno partida 2)
(set-estado-ganador partida 'ash)