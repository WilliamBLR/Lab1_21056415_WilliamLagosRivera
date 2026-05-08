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



(provide CARD-TYPE
         card-type?
         ELEMENT-TYPE
         element-type?
         ENERGY)




;Enumeración de tipos de carta
(define CARD-TYPE '(pokemon energy trainer))
;Verifica si el simbolo ingresado corresponde a una carta valida
;dom: t (simbolo)
;rec: un booleano
;tipo de recursion: no aplica

(define (card-type? t)
  (and (symbol? t)
       (member t CARD-TYPE)))
 
;Enumeración de elementos
;lista de todos los elementos disponibles
(define ELEMENT-TYPE '(grass fire water lightning psychic fighting darkness metal colorless fairy))
(define (element-type? e)
  (and (symbol? e)
       (member e ELEMENT-TYPE)))
 
;lista de pares que asocial el nombre con un elemento
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


