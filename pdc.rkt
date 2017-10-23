#! /usr/bin/env racket
#reader(lib "htdp-advanced-reader.ss" "lang")((modname pdc) (
  read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Función que crea el tablero
(define (crear_Tablero rows&cols)(
  cond ((equal? rows&cols 1) '(0))(
    else (crear_Tablero_Aux rows&cols rows&cols '())
    )
  ))

; Funcion auxiliar para crear el tablero
(define (crear_Tablero_Aux num_of_rc cont res)(
  cond ((equal? cont 0) res) (
    else (crear_Tablero_Aux num_of_rc (- cont 1) (append res (list
      (mk_List num_of_rc '())) ))
    )
  ))

; Funcion que crea la una lista llena de 0s
(define (mk_List num_elemt rList)(
  if (equal? num_elemt 0) rList (
    mk_List (- num_elemt 1) (append rList (list 0))
    )
  ))

;Funcion que modifica el valor de una casilla en la matris
(define (change_Value matrix row col value)(
  cond ((null? matrix) '())(
    else (changer_Aux matrix row col '() value)
    )
  ))

;Funcion auxiliar para modificar valores en una matris
(define (changer_Aux matrix row col res value)(
  cond ((not (equal? row 0)) (changer_Aux (cdr matrix) (- row 1) col (
         append res (list (car matrix))) value))
       ((and (equal? row 0) (not (equal? col 0))) (append (append res (
         list (susti value col (car matrix) '()))) (cdr matrix)))
  ))

;Funcion que modifica un valor en una lista
(define (susti value pos lista res)(
  cond ((equal? pos 0) (append res (append (list value) (cdr lista))))(
    else (susti value (- pos 1) (cdr lista) (append res (list (car lista))))
    )
  ))


;(define table '((1 2 3) (4 5 6) (7 8 9) (10 11 12) (13 14 15)))
;(define table (crear_Tablero 3))
;(write table)
(change_Value table 1 1 69)
