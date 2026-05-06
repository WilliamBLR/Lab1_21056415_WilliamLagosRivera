#lang racket
;RF02
(require "21056415-2base.rkt")

(provide attack
         attack?
         attack-cost
         attack-name
         attack-text
         attack-function)

#|

Representación:
(list 'attack cost name text function)

- cost: Lista de símbolos (ej: '(psychic colorless))
- name: String con el nombre del ataque[cite: 4]
- text: String con la descripción del ataque[cite: 4]
- function: Función de orden superior que ejecuta el daño/efecto[cite: 4]
|#

;; Constructor
(define (attack cost name text function)
  (if (and (list? cost)
           (string? name)
           (string? text)
           (procedure? function)) ; Verifica que el 4to argumento sea una función[cite: 4]
      (list 'attack cost name text function)
      #f))

;; Función de pertenencia
(define (attack? a)
  (and (list? a)
       (= (length a) 5)
       (equal? (car a) 'attack)
       (list? (cadr a))
       (string? (caddr a))
       (string? (cadddr a))
       (procedure? (list-ref a 4))))

;; Selectores
(define (attack-cost a)
  (if (attack? a)
      (cadr a)
      #f))

(define (attack-name a)
  (if (attack? a)
      (caddr a)
      #f))

(define (attack-text a)
  (if (attack? a)
      (cadddr a)
      #f))

(define (attack-function a)
  (if (attack? a)
      (list-ref a 4)
      #f))