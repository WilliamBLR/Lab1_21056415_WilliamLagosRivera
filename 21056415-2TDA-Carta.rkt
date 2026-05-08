#lang racket
;RF03
;TDA CARTA
;Energia3, entrenador 5, Pokemon 11

(require "21056415-2base.rkt")
(require "21056415-2TDA-Ataque.rkt") 

(provide card
         card?
         card-tipo
         card-nombre
         card-energia-elemento
         card-trainer-subtipo
         card-texto
         card-funcion
         card-evoluciona-de
         card-ps
         card-pokemon-elemento
         card-debilidad
         card-resistencia
         card-coste-retirada
         card-es-ex?
         card-habilidad
         card-ataques
         carta-pokemon?
         carta-energy?
         carta-trainer?
         pokemon-energias
         pokemon-dano
         pokemon-estado
         add-pokemon-energia)

;; Dom: tipoCarta (CARD-TYPE) X nombre (string) X argumentos-variables (. resto)
;; Rec: lista (representación del TDA Carta) o #f si falla la validación

;Recibe una cantidad X de  datos y dependiendo de de la cartidta, valida ciertas cosas
(define (card tipoCarta nombre . resto)
  (cond
    ;;Carta energia
    [(equal? tipoCarta 'energy)
     (if (and (= (length resto) 1)
              (string? nombre)
              (element-type? (car resto)))
         (list 'card tipoCarta nombre (car resto))
         #f)]

    ;;Carta entrenador
    [(equal? tipoCarta 'trainer)
     (if (and (= (length resto) 3)
              (string? nombre)
              (or (equal? (car resto) "objeto") (equal? (car resto) "partidario"))
              (string? (cadr resto))
              (procedure? (caddr resto)))
         (list 'card tipoCarta nombre (car resto) (cadr resto) (caddr resto))
         #f)]

    ;;Carta pokmeon, al ser 11 datos es mas complejo de analizar
    [(equal? tipoCarta 'pokemon)
     (if (and (= (length resto) 9)
              
              (string? nombre)
              
              (or (string? (car resto)) (null? (car resto)))
              
              (number? (cadr resto)) (> (cadr resto) 0)
              
              (element-type? (caddr resto))
              
              (or (element-type? (cadddr resto)) (null? (cadddr resto)))
              
              (or (element-type? (list-ref resto 4)) (null? (list-ref resto 4)))
              
              (or (number? (list-ref resto 5)) (null? (list-ref resto 5)))
              
              (boolean? (list-ref resto 6))
              
              (or (attack? (list-ref resto 7)) (null? (list-ref resto 7)))
              
              (list? (list-ref resto 8))
              
              
              (if (null? (list-ref resto 7))
                  (<= (length (list-ref resto 8)) 3)
                  (<= (length (list-ref resto 8)) 2)))
         
         ;Se le agregan internamente los campos para energias, dano y estado (RF11)
         (append (list 'card tipoCarta nombre) resto (list '() 0 "normal"))
         #f)]
    [else #f]))


;; Dom:
;; Rec: booleano

;Funcion que tan solo verifica que esto sea una lista
(define (card? c)
  (and (list? c)
       (>= (length c) 4) 
       (equal? (car c) 'card)
       (card-type? (cadr c))))

;Extraccion del tipo de carta
(define (card-tipo c) (if (card? c) (list-ref c 1) #f))
;Extraccion del nombre de la carta
(define (card-nombre c) (if (card? c) (list-ref c 2) #f))
;Extraccion del elemeneto
(define (card-energia-elemento c) (if (and (card? c) (equal? (card-tipo c) 'energy)) (list-ref c 3) #f))
;En caso de ser carta entrenador, verifica que sea objetio o partidario

;Cada extractor trae su nombre, no explicare cada uno
(define (card-trainer-subtipo c) (if (and (card? c) (equal? (card-tipo c) 'trainer)) (list-ref c 3) #f))

(define (card-texto c) (if (and (card? c) (equal? (card-tipo c) 'trainer)) (list-ref c 4) #f))

(define (card-funcion c) (if (and (card? c) (equal? (card-tipo c) 'trainer)) (list-ref c 5) #f))

(define (card-evoluciona-de c) (if (and (card? c) (equal? (card-tipo c) 'pokemon)) (list-ref c 3) #f))

(define (card-ps c) (if (and (card? c) (equal? (card-tipo c) 'pokemon)) (list-ref c 4) #f))

(define (card-pokemon-elemento c) (if (and (card? c) (equal? (card-tipo c) 'pokemon)) (list-ref c 5) #f))

(define (card-debilidad c) (if (and (card? c) (equal? (card-tipo c) 'pokemon)) (list-ref c 6) #f))

(define (card-resistencia c) (if (and (card? c) (equal? (card-tipo c) 'pokemon)) (list-ref c 7) #f))

(define (card-coste-retirada c) (if (and (card? c) (equal? (card-tipo c) 'pokemon)) (list-ref c 8) #f))

(define (card-es-ex? c) (if (and (card? c) (equal? (card-tipo c) 'pokemon)) (list-ref c 9) #f))

(define (card-habilidad c) (if (and (card? c) (equal? (card-tipo c) 'pokemon)) (list-ref c 10) #f))

(define (card-ataques c) (if (and (card? c) (equal? (card-tipo c) 'pokemon)) (list-ref c 11) #f))


;Funciones que cree en mi RF01 para el TDA, sin embargo con la re-estructuracion que hice en el RF03 reemplazare el TDA carta pasado, por este
;Estas funciones las herede de ahi
(define (carta-pokemon? c)
  (and (card? c)
       (equal? (card-tipo c) 'pokemon)))

(define (carta-energy? c)
  (and (card? c)
       (equal? (card-tipo c) 'energy)))

(define (carta-trainer? c)
  (and (card? c)
       (equal? (card-tipo c) 'trainer)))


;RF11 (cambios) :c
;Extrae la lista de energias asociadas al pokemon
(define (pokemon-energias p) (if (carta-pokemon? p) (list-ref p 12) #f))
;Extrae el dano acumulado del pokemon
(define (pokemon-dano p) (if (carta-pokemon? p) (list-ref p 13) #f))
;Extrae el estado actual del pokemon
(define (pokemon-estado p) (if (carta-pokemon? p) (list-ref p 14) #f))

;cambio tambien
(define (add-pokemon-energia p energia)
  (if (and (carta-pokemon? p) (carta-energy? energia))
      (append (take p 12) 
              (list (append (pokemon-energias p) (list energia))) 
              (drop p 13))
      #f))












