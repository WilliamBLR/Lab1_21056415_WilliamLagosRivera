#lang racket

(require "21056415-2base.rkt")
(require "21056415-2TDA-Carta.rkt")
(require "21056415-2TDA-Jugador.rkt")
(require "21056415-2TDA-Juego.rkt")

(provide playToBench)



;elimina la primera ocurrencia de una carta especifica en una lista
(define (quitar-carta-mano carta mano)
  (if (null? mano)
      '()
      (if (equal? carta (car mano))
          (cdr mano)
          (cons (car mano) (quitar-carta-mano carta (cdr mano))))))

;verifica si una carta realmente existe dentro de la lista de la mano
(define (carta-en-mano? carta mano)
  (if (null? mano)
      #f
      (if (equal? carta (car mano))
          #t
          (carta-en-mano? carta (cdr mano)))))

;funcion rf08 

;descripcion: mueve un pokemon de la mano a la banca del jugador actual si hay espacio
;dom: juego x carta
;rec: juego
(define (playToBench juego carta)
  (if (and (juego? juego) (carta-pokemon? carta))
      (let* ((turno (juego-turno juego))
             (jugador-actual (if (= turno 1) (juego-jugador1 juego) (juego-jugador2 juego)))
             (mano (jugador-mano jugador-actual))
             (banca (jugador-banca jugador-actual)))
        
        ;se valida que la carta este en la mano y que la banca tenga menos de 5 pokemon
        (if (and (carta-en-mano? carta mano) (< (length banca) 5))
            (let* ((nueva-mano (quitar-carta-mano carta mano))
                   (nueva-banca (append banca (list carta)))
                   ;se actualizan ambas zonas en el jugador
                   (jugador-actualizado (set-jugador-banca 
                                         (set-jugador-mano jugador-actual nueva-mano) 
                                         nueva-banca)))
              
              ;se devuelve el juego con el jugador actualizado segun de quien sea el turno
              (if (= turno 1)
                  (set-juego-jugador1 juego jugador-actualizado)
                  (set-juego-jugador2 juego jugador-actualizado)))
            juego)) ;si la banca esta llena o la carta no esta en mano retorna el juego intacto
      juego))