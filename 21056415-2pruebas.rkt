#lang racket
#|

Laboratorio 1 - Scheme - Paradigmas de Programacion
Nombre: William Lagos Rivera
Profesor: Edmundo Leiva

RF01: Especificar cada TDA
- carta
- criatura
- energia
- accion
- jugador
- estado del juego

|#





(carta-trainer? c3)

;; Enumeración de tipos de carta
(define CARD-TYPE '(pokemon energy trainer))
(define (card-type? t)
  (and (symbol? t)
       (member t CARD-TYPE)))
 
;; Enumeración de elementos
(define ELEMENT-TYPE '(grass fire water lightning psychic fighting darkness metal colorless fairy))
(define (element-type? e)
  (and (symbol? e)
       (member e ELEMENT-TYPE)))
 
;; Lista de nombres de energía con su tipo.
;; Esta lista base se puede ajustar con otros nombres de ataques y daños.
;; Los tipos se limitan a los indicados por ELEMENT-TYPE
(define ENERGY
  '((fire-energy      . fire)
    (water-energy     . water)
    (grass-energy     . grass)
    (lightning-energy . lightning)
    (psychic-energy   . psychic)
    (fighting-energy  . fighting)
    (darkness-energy  . darkness)
    (metal-energy     . metal)
    (fairy-energy     . fairy)
    (colorless-energy . colorless)))







