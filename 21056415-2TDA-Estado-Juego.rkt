#lang racket

(require "21056415-2TDA-Jugador.rkt")

(provide make-estado-juego
         estado-juego?
         estado-juego-jugador1
         estado-juego-jugador2
         estado-juego-turno
         estado-juego-ganador
         set-estado-turno
         set-estado-ganador)

#|

Representacion:
(list 'estado-juego jugador1 jugador2 turno ganador)

Donde:
- 'estado-juego identifica el TDA.
- jugador1 es un jugador valido.
- jugador2 es un jugador valido.
- turno indica el numero de turno actual.
- ganador guarda el ganador si existe, o '() si aun no hay ganador.
|#

;;Constructor

;verificacionessss
(define (make-estado-juego jugador1 jugador2 turno ganador)
  (if (and (jugador? jugador1)
           (jugador? jugador2)
           (number? turno)
           (>= turno 1)
           (or (symbol? ganador)
               (equal? ganador '())))
      (list 'estado-juego jugador1 jugador2 turno ganador)
      #f))

;;Funcion de pertenencia
(define (estado-juego? e)
  (and (list? e)
       (= (length e) 5)
       (equal? (car e) 'estado-juego)
       (jugador? (cadr e))
       (jugador? (caddr e))
       (number? (cadddr e))
       (>= (cadddr e) 1)
       (or (symbol? (list-ref e 4))
           (equal? (list-ref e 4) '()))))

;;Selectores
(define (estado-juego-jugador1 e)
  (if (estado-juego? e)
      (cadr e)
      #f))

(define (estado-juego-jugador2 e)
  (if (estado-juego? e)
      (caddr e)
      #f))

(define (estado-juego-turno e)
  (if (estado-juego? e)
      (cadddr e)
      #f))

(define (estado-juego-ganador e)
  (if (estado-juego? e)
      (list-ref e 4)
      #f))

;;Modificadores
(define (set-estado-turno e nuevo-turno)
  (if (and (estado-juego? e)
           (number? nuevo-turno)
           (>= nuevo-turno 1))
      (make-estado-juego (estado-juego-jugador1 e)
                         (estado-juego-jugador2 e)
                         nuevo-turno
                         (estado-juego-ganador e))
      #f))

(define (set-estado-ganador e nuevo-ganador)
  (if (and (estado-juego? e)
           (or (symbol? nuevo-ganador)
               (equal? nuevo-ganador '())))
      (make-estado-juego (estado-juego-jugador1 e)
                         (estado-juego-jugador2 e)
                         (estado-juego-turno e)
                         nuevo-ganador)
      #f))


