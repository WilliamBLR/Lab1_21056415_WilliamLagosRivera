#lang racket





;Extraigo cada una de las funciones dadas en el enunciado
#|


Funciones que me dan en mi archivo



|#
(require "pruebas.rkt") 



;Funciones que llevare a mi archivo base, es decir, creare en mi TDA todas estas funciones que seran utilizadas en el futuro
(provide make-carta
         carta?
         carta-tipo
         carta-contenido
         set-carta-tipo
         set-carta-contenido
         carta-pokemon?
         carta-energy?
         carta-trainer?



)

;;
;; TDA carta
;; (list 'carta tipo contenido)
;; Donde:
;; - 'carta identifica el TDA.
;; - tipo pertenece a CARD-TYPE: pokemon, energy o trainer.
;; - contenido guarda la informacion especifica de la carta.
;; =========================================================

;;Lo primero que vamos a definior es el constructor del TDA carta
(define (make-carta tipo contenido)
  (if (card-type? tipo)
      (list 'carta tipo contenido)
      #f))

;;Funcion de pertenencia
(define (carta? c)
  (and (list? c)
       (= (length c) 3)
       (equal? (car c) 'carta)
       (card-type? (cadr c))))

;;Selectores
(define (carta-tipo c)
  (if (carta? c)
      (cadr c)
      #f))

(define (carta-contenido c)
  (if (carta? c)
      (caddr c)
      #f))

;;Modificadores

(define (set-carta-tipo c nuevo-tipo)
  (if (and (carta? c)
           (card-type? nuevo-tipo))
      (make-carta nuevo-tipo (carta-contenido c))
      #f))

(define (set-carta-contenido c nuevo-contenido)
  (if (carta? c)
      (make-carta (carta-tipo c) nuevo-contenido)
      #f))

;;Otras funciones

(define (carta-pokemon? c)
  (and (carta? c)
       (equal? (carta-tipo c) 'pokemon)))

(define (carta-energy? c)
  (and (carta? c)
       (equal? (carta-tipo c) 'energy)))

(define (carta-trainer? c)
  (and (carta? c)
       (equal? (carta-tipo c) 'trainer)))