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
  cond ((equal? num_elemt 0) rList)(
    else (mk_List (- num_elemt 1) (append rList (list 0)))
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
         list (susti value col (car matrix) '()))) (cdr matrix)))(
           else (append (list (susti value col (car matrix) '())) (cdr matrix))
           )
  ))

;Funcion que modifica un valor en una lista
(define (susti value pos lista res)(
  cond ((equal? pos 0) (append res (append (list value) (cdr lista))))(
    else (susti value (- pos 1) (cdr lista) (append res (list (car lista))))
    )
  ))

;Funcion que obtiene el valor de una lista
(define (get_Val matrix row col)(
  cond ((not (equal? row 0)) (get_Val (cdr matrix) (- row 1) col))
       ((and (equal? row 0) (not (equal? col 0))) (get_ValAux col (car matrix)))
       (else (caar matrix)
         )
  ))

;Funcion auxiliar para obtener el valor
(define (get_ValAux pos lista)(
  cond ((equal? pos 0) (car lista))(
    else (get_ValAux (- pos 1) (cdr lista))
    )
  ))

;Funcion que devuelve una matriz con los posibles movimientos  del caballo
(define (generate_legal_moves actual posible_movs size step)(
  cond ((or (>= (car actual) size) (>= (cadr actual) size) (< (car actual) 0)
    (< (cadr actual) 0)) '())
       ((>= step size) posible_movs)
       ((and (equal? step 0) (<= 0 (+ (car actual) 1))
         (<= 0 (+ (cadr actual) 2)) (< (+ (car actual) 1) size)
           (< (+ (cadr actual) 2) size)) (generate_legal_moves actual
             (append posible_movs (list '(1 2))) size (+ step 1)))
       ((and (equal? step 1) (<= 0 (+ (car actual) 1))
         (<= 0 (- (cadr actual) 2)) (< (+ (car actual) 1) size)
           (< (- (cadr actual) 2) size)) (generate_legal_moves actual
             (append posible_movs (list '(1 -2))) size (+ step 1)))
       ((and (equal? step 2) (<= 0 (- (car actual) 1))
         (<= 0 (+ (cadr actual) 2)) (< (- (car actual) 1) size)
           (< (+ (cadr actual) 2) size)) (generate_legal_moves actual
             (append posible_movs (list '(-1 2))) size (+ step 1)))
       ((and (equal? step 3) (<= 0 (- (car actual) 1))
         (<= 0 (- (cadr actual) 2)) (< (- (car actual) 1) size)
           (< (- (cadr actual) 2) size)) (generate_legal_moves actual
             (append posible_movs (list '(-1 -2))) size (+ step 1)))
       ((and (equal? step 4) (<= 0 (+ (car actual) 2))
         (<= 0 (+ (cadr actual) 1)) (< (+ (car actual) 2) size)
           (< (+ (cadr actual) 1) size)) (generate_legal_moves actual
             (append posible_movs (list '(2 1))) size (+ step 1)))
       ((and (equal? step 5) (<= 0 (+ (car actual) 2))
         (<= 0 (- (cadr actual) 1)) (< (+ (car actual) 2) size)
           (< (- (cadr actual) 1) size)) (generate_legal_moves actual
             (append posible_movs (list '(2 -1))) size (+ step 1)))
       ((and (equal? step 6) (<= 0 (- (car actual) 2))
         (<= 0 (+ (cadr actual) 1)) (< (- (car actual) 2) size)
           (< (+ (cadr actual) 1) size)) (generate_legal_moves actual
             (append posible_movs (list '(-2 1))) size (+ step 1)))
       ((and (equal? step 7) (<= 0 (- (car actual) 2))
         (<= 0 (- (cadr actual) 1)) (< (- (car actual) 2) size)
           (< (- (cadr actual) 1) size)) (generate_legal_moves actual
             (append posible_movs (list '(-2 -1))) size (+ step 1)))(
         else (generate_legal_moves actual posible_movs size (+ step 1))
         )
  ))

;Funcion para eliminar el ultimo elemento de una lista
(define (deLast lista lar)(
  cond ((equal? lar 1) '())(
    else (delAux lista lar '())
    )
  ))

;Funcion auxiliar para eliminar el ultimo elemento
(define (delAux lista lar res)(
  cond ((equal? lar 1) res)(
    else (delAux (cdr lista) (- lar 1) (append res (list (car lista))))
    )
  ))

;(define table '((1 2 3) (4 5 6) (7 8 9) (10 11 12) (13 14 15)))
;(define table (crear_Tablero 8))
;(write table)
