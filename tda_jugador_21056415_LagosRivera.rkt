#lang racket

(require "base_21056415_LagosRivera.rkt")
(require "tda_carta_21056415_LagosRivera.rkt")
(require "tda_criatura_21056415_LagosRivera.rkt")

(provide make-jugador
         jugador?
         jugador-nombre
         jugador-mazo
         jugador-mano
         jugador-activo
         jugador-banca
         jugador-descarte
         jugador-premios
         set-jugador-mazo
         set-jugador-mano
         set-jugador-activo
         set-jugador-banca
         set-jugador-premios)

#|


representacion que utilizo en este caso:
(list 'jugador nombre mazo mano activo banca descarte premios)
|#

;constructor

;crea un jugador validando cada uno de sus componentes iniciales
(define (make-jugador nombre mazo mano activo banca descarte premios)
  (if (and (string? nombre) ;verifico que sea un string
           (list? mazo) ;verifico que seea una lista

           ;mismo proceso con todo
           
           (list? mano) 
           (or (carta-pokemon? activo) (null? activo)) ;modificacion o correcion para el RF08
           (list? banca)
           (list? descarte)
           (list? premios))
      (list 'jugador nombre mazo mano activo banca descarte premios)
      #f))

;;pertenencia

;verifica que la estructura sea una lista de jugador valida con 8 elementos
(define (jugador? j)
  (and (list? j)
       (= (length j) 8)
       (equal? (car j) 'jugador)
       (string? (cadr j))
       (list? (caddr j))
       (list? (list-ref j 3))
       (list? (list-ref j 5))
       (list? (list-ref j 6))
       (list? (list-ref j 7))))

;;selectores

;obtiene el nombre del jugador
(define (jugador-nombre j)
  (if (jugador? j)
      (cadr j)
      #f))

;obtiene el mazo del jugador
(define (jugador-mazo j)
  (if (jugador? j)
      (caddr j)
      #f))

;obtiene la lista de cartas en la mano
(define (jugador-mano j)
  (if (jugador? j)
      (list-ref j 3)
      #f))

;obtiene la criatura que esta en el puesto activo
(define (jugador-activo j)
  (if (jugador? j)
      (list-ref j 4)
      #f))

;obtiene la lista de criaturas en la banca
(define (jugador-banca j)
  (if (jugador? j)
      (list-ref j 5)
      #f))

;obtiene la pila de cartas descartadas
(define (jugador-descarte j)
  (if (jugador? j)
      (list-ref j 6)
      #f))

;obtiene la lista de cartas de premio
(define (jugador-premios j)
  (if (jugador? j)
      (list-ref j 7)
      #f))

;;modificadores

;a nivel general el dom de cada funcion sera un jugador x nuevo mazo
; el rec tambien sera algo general, donde sera la lista jugador o falso

;actualiza el mazo del jugador
(define (set-jugador-mazo j nuevo-mazo)
  (if (and (jugador? j) (list? nuevo-mazo))
      (make-jugador (jugador-nombre j)
                    nuevo-mazo
                    (jugador-mano j)
                    (jugador-activo j)
                    (jugador-banca j)
                    (jugador-descarte j)
                    (jugador-premios j))
      #f))

;actualiza la mano del jugador
(define (set-jugador-mano j nueva-mano)
  (if (and (jugador? j) (list? nueva-mano))
      (make-jugador (jugador-nombre j)
                    (jugador-mazo j)
                    nueva-mano
                    (jugador-activo j)
                    (jugador-banca j)
                    (jugador-descarte j)
                    (jugador-premios j))
      #f))

;actualiza la criatura activa
(define (set-jugador-activo j nuevo-activo)
  (if (and (jugador? j) (or (carta-pokemon? nuevo-activo) (null? nuevo-activo))) ; 
      (make-jugador (jugador-nombre j)
                    (jugador-mazo j)
                    (jugador-mano j)
                    nuevo-activo
                    (jugador-banca j)
                    (jugador-descarte j)
                    (jugador-premios j))
      #f))

;actualiza la banca del jugador
(define (set-jugador-banca j nueva-banca)
  (if (and (jugador? j) (list? nueva-banca))
      (make-jugador (jugador-nombre j)
                    (jugador-mazo j)
                    (jugador-mano j)
                    (jugador-activo j)
                    nueva-banca
                    (jugador-descarte j)
                    (jugador-premios j))
      #f))

;actualiza los premios del jugador
(define (set-jugador-premios j nuevos-premios)
  (if (and (jugador? j) (list? nuevos-premios))
      (make-jugador (jugador-nombre j)
                    (jugador-mazo j)
                    (jugador-mano j)
                    (jugador-activo j)
                    (jugador-banca j)
                    (jugador-descarte j)
                    nuevos-premios)
      #f))