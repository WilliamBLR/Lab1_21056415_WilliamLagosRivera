#lang racket

(require "21056415-2TDA-Jugador.rkt")
(require "21056415-2TDA-Accion.rkt")
(require "21056415-2TDA-Criatura.rkt")
(require "21056415-2TDA-Energia.rkt")
(require "21056415-2TDA-Estado-Juego.rkt")
(require "21056415-2TDA-Usar-Accion.rkt")
(require "21056415-2TDA-Ataque.rkt")

(define pikachu
  (make-criatura 'pikachu
                 'lightning
                 60
                 60
                 (list (make-ataque 'impactrueno
                                   (list (make-energia 'lightning-energy))
                                   30
                                   'paralizar))
                 '()
                 'normal
                 1))

(define misty
  (make-jugador 'misty '() '() '() '() '() '()))

(define partida
  (make-estado-juego pikachu misty 1 '()))

(define ataque1
  (make-ataque 'impactrueno
               (list (make-energia 'lightning-energy))
               30
               'paralizar))

(define ataque-accion
  (make-usar-accion pikachu misty ataque1))

;; Verificando los datos de la acción
(displayln (usar-accion-atacante ataque-accion))  ; pikachu
(displayln (usar-accion-defensor ataque-accion))  ; misty
(displayln (usar-accion-accion ataque-accion))  ; impactrueno