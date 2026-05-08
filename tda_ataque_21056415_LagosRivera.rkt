#lang racket
;RF02
(require "base_21056415_LagosRivera.rkt")

(provide attack
         attack?
         attack-cost
         attack-name
         attack-text
         attack-function)

#|

Representacion que usare en este caso:
(list 'attack cost name text function)
donde:
- cost: Lista de simbolos 
- name: String con el nombre del ataque
- text: String con la descripción del ataque
- function: Función de orden superior que ejecuta el daño u efecto, puede ser cualquier de las 2, sera definido luego
|#

;Constructor
; dom: costo (lista) x nombre (string) x texto(string)
;rec: lista(que en este caso es el tda ataque) o puede ser un booleano falso
;tipo de recursion: no aplica en este caso

;no hay mucho que explicar en esta fucnion, vamos a ir verificando que los datos sean correctos
(define (attack cost name text function)
  (if (and (list? cost)
           (string? name)
           (string? text)
           (procedure? function)) ; Verifica que el 4to argumento sea una funcion
      (list 'attack cost name text function)
      #f))

;Función de pertenencia que va ir verificando si la estructura de ataque es correcta o no
;rec: booleano
;tipo de recursion: no aplicable en este caso

;mas verificaciones
(define (attack? a)
  (and (list? a)
       (= (length a) 5)
       (equal? (car a) 'attack)
       (list? (cadr a))
       (string? (caddr a))
       (string? (cadddr a))
       (procedure? (list-ref a 4))))

;SELECTORES

;selector que obtiene el costo del ataque
;dom: ataque
;rec: lista o falso

(define (attack-cost a)
  (if (attack? a)
      (cadr a)
      #f))

;selector que obtiene el nombre
;dom: ataque
;rec; string o falso
;no aplica recursion

(define (attack-name a)
  (if (attack? a)
      (caddr a)
      #f))


;selector 1: como su nombre lo dice, va ir seleccionando o extrayendo la descripcion del ataque
;Dom: ataque
;rec: string o un booleano falso en caso de ir verificando
;no aplica el tipo de recursion
(define (attack-text a)
  (if (attack? a)
      (cadddr a)
      #f))

(define (attack-function a)
  (if (attack? a)
      (list-ref a 4)
      #f))