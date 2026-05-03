#lang racket
;Archivo solo para pruebas de cada TDA, se iran borrando y agregando cosas a medida que vaya probando
(require "21056415-2TDA-Accion.rkt")
(require "21056415-2TDA-Energia.rkt")

(define a1 (make-accion 'impactrueno
                        (list (make-energia 'lightning-energy))
                        30
                        'paralizar))

a1
(accion? a1)
(accion-nombre a1)
(accion-costo a1)
(accion-dano a1)
(accion-efecto a1)
(set-accion-dano a1 50)




