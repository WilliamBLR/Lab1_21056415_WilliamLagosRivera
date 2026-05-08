#lang racket


(require "21056415-2base.rkt")
(require "21056415-2TDA-Carta.rkt")
(require "21056415-2TDA-Deck.rkt")
(require "21056415-2TDA-Jugador.rkt")
(require "21056415-2TDA-Juego.rkt")

(provide printGame)

;--- funciones auxiliares ---

;descripcion: convierte un dato a string de forma segura para evitar errores de tipo
(define (seguro->string dato)
  (cond [(string? dato) dato]
        [(symbol? dato) (symbol->string dato)]
        [(number? dato) (number->string dato)]
        [(null? dato) "ninguno"]
        [else "desconocido"]))

;recursion de cola

;descripcion: concatena los nombres de una lista de cartas
;tipo recursion: cola
(define (nombres-cartas-cola lista acc)
  (if (null? lista)
      (if (equal? acc "") "vacia" acc)
      (nombres-cartas-cola (cdr lista)
                           (string-append acc
                                          (if (equal? acc "") "" ", ")
                                          (seguro->string (list-ref (car lista) 2)))))) ;posicion 2 es el nombre en rf03

;descripcion: formatea los datos de todos los pokemon de la banca
;tipo recursion: cola
(define (banca-cola lista acc)
  (if (null? lista)
      (if (equal? acc "") "  ninguno\n" acc)
      (banca-cola (cdr lista)
                  (string-append acc "  - " (format-pokemon (car lista))))))



;descripcion: formatea los datos de un pokemon


;Parte del codigo comentada para correcion de error con respecto al RF11, reconstruire esa funcion para leer de manera correcta
;el daño y el estado real de la carta

#|(define (format-pokemon p)
  (if (or (null? p) (not p))
      "ninguno\n"
      (string-append (seguro->string (list-ref p 2)) " "
                     (seguro->string (list-ref p 4)) "PS "
                     "tipo: " (seguro->string (list-ref p 5)) " "
                     (if (null? (list-ref p 3)) "(basico)" "(evolucion)") "\n"
                     "    debilidad: " (seguro->string (list-ref p 6))
                     " | resistencia: " (seguro->string (list-ref p 7)) "\n"
                     "    energias: 0 | daño: 0 | estado: normal\n")))|#




;descripcion: formatea los datos de un pokemon
(define (format-pokemon p)
  (if (or (null? p) (not p))
      "ninguno\n"
      (string-append (seguro->string (list-ref p 2)) " "
                     (seguro->string (list-ref p 4)) "PS "
                     "tipo: " (seguro->string (list-ref p 5)) " "
                     (if (null? (list-ref p 3)) "(basico)" "(evolucion)") "\n"
                     "    debilidad: " (seguro->string (list-ref p 6))
                     " | resistencia: " (seguro->string (list-ref p 7)) "\n"
                     "    energias: " (number->string (length (list-ref p 12))) ; En los 3 comentarios siguiente estan las modificaciones con respecto al codigo anterior
                     " | daño: " (number->string (list-ref p 13))               ; 
                     " | estado: " (list-ref p 14) "\n")))                      ; 

;descripcion: construye el string con la vista completa de un jugador
;descripcion: construye el string con la vista completa de un jugador
(define (format-jugador j num-jugador-solicitado num-jugador-actual)
  (let ((mostrar-mano? (= num-jugador-solicitado num-jugador-actual)))
    (let ((mano (jugador-mano j))
          (mazo-len (length (deck-cartas (jugador-mazo j))))
          (premios-len (length (jugador-premios j)))
          (descarte (jugador-descarte j))
          (activo (jugador-activo j))
          (banca (jugador-banca j)))
      (string-append "=======================================================\n"
                     "JUGADOR " (number->string num-jugador-actual) ": " (jugador-nombre j) "\n"
                     "cartas en mazo: " (number->string mazo-len) " disponibles\n"
                     "cartas de premio: " (number->string premios-len) " ocultas\n"
                     "cartas en mano (" (number->string (length mano)) "): "
                     (if mostrar-mano?
                         (string-append "\n  " (nombres-cartas-cola mano "") "\n")
                         "ocultas\n")
                     "pokemon activo:\n  " (format-pokemon activo)
                     "banca:\n" (banca-cola banca "")
                     "pila de descarte: " (nombres-cartas-cola descarte "") "\n"
                     "=================================================\n"))))

;funcion principal rf07

;descripcion: retorna un string con la representacion visual del juego
;dom: juego X int
;rec: string
(define (printGame juego num-jugador)
  (if (juego? juego)
      (string-append "TURNO ACTUAL: JUGADOR " (number->string (juego-turno juego)) "\n\n"
                     (format-jugador (juego-jugador1 juego) num-jugador 1)
                     "\n"
                     (format-jugador (juego-jugador2 juego) num-jugador 2))
      "error: juego invalido"))