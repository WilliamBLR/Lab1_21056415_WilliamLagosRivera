#lang racket
;RF02
;tda usar accion
(require "tda_jugador_21056415_LagosRivera.rkt")
(require "tda_criatura_21056415_LagosRivera.rkt")
(require "tda_accion_21056415_LagosRivera.rkt")
(require "tda_energia_21056415_LagosRivera.rkt")
(require "tda_estadojuego_21056415_LagosRivera.rkt")
(require "tda_ataque_21056415_LagosRivera.rkt")

(provide make-usar-accion
         usar-accion?
         usar-accion-atacante
         usar-accion-defensor
         usar-accion-accion
         set-usar-accion-resultado)

#|
Debe guardar:
- atacante: la criatura que realiza el ataque
- defensor: la criatura que recibe el ataque
- accion: el ataque usado

Representación:
(list 'usar-accion atacante defensor accion)
|#

;Constructor
(define (make-usar-accion atacante defensor accion)
  (if (and (criatura? atacante)
           (criatura? defensor)
           (attack? accion)) ; Actualizacion que hice en el commit ya que utilice el TDA ataque o attack
      (list 'usar-accion atacante defensor accion)
      #f))

;Función de pertenencia
(define (usar-accion? u)
  (and (list? u)
       (= (length u) 4)
       (equal? (car u) 'usar-accion)
       (criatura? (cadr u))
       (criatura? (caddr u))
       (attack? (list-ref u 3))))

;Selectores
(define (usar-accion-atacante u)
  (if (usar-accion? u)
      (cadr u)
      #f))

(define (usar-accion-defensor u)
  (if (usar-accion? u)
      (caddr u)
      #f))

(define (usar-accion-accion u)
  (if (usar-accion? u)
      (list-ref u 3)
      #f))

;Modificadores
(define (set-usar-accion-resultado u resultado)
  (if (usar-accion? u)
      (make-usar-accion (usar-accion-atacante u)
                        (usar-accion-defensor u)
                        (usar-accion-accion u))
      #f))