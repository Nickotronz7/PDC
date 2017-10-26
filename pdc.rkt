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
  cond ((not (equal? row 0)) (changer_Aux (cdr matrix) (- row 1) col
    (append res (list (car matrix))) value))
       ((and (equal? row 0) (not (equal? col 0))) (append (append res
         (list (susti value col (car matrix) '()))) (cdr matrix)))(
           else (append (append res (list (susti value col (car matrix) '())))
             (cdr matrix))
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
             (append posible_movs (list (list (+ (car actual) 1) (+ (cadr actual) 2)))) size (+ step 1)))
       ((and (equal? step 1) (<= 0 (+ (car actual) 1))
         (<= 0 (- (cadr actual) 2)) (< (+ (car actual) 1) size)
           (< (- (cadr actual) 2) size)) (generate_legal_moves actual
             (append posible_movs (list (list (+ (car actual) 1) (- (cadr actual) 2)))) size (+ step 1)))
       ((and (equal? step 2) (<= 0 (- (car actual) 1))
         (<= 0 (+ (cadr actual) 2)) (< (- (car actual) 1) size)
           (< (+ (cadr actual) 2) size)) (generate_legal_moves actual
             (append posible_movs (list (list (- (car actual) 1) (+ (cadr actual) 2)))) size (+ step 1)))
       ((and (equal? step 3) (<= 0 (- (car actual) 1))
         (<= 0 (- (cadr actual) 2)) (< (- (car actual) 1) size)
           (< (- (cadr actual) 2) size)) (generate_legal_moves actual
             (append posible_movs (list (list (- (car actual) 1) (- (cadr actual) 2)))) size (+ step 1)))
       ((and (equal? step 4) (<= 0 (+ (car actual) 2))
         (<= 0 (+ (cadr actual) 1)) (< (+ (car actual) 2) size)
           (< (+ (cadr actual) 1) size)) (generate_legal_moves actual
             (append posible_movs (list (list (+ (car actual) 2) (+ (cadr actual) 1)))) size (+ step 1)))
       ((and (equal? step 5) (<= 0 (+ (car actual) 2))
         (<= 0 (- (cadr actual) 1)) (< (+ (car actual) 2) size)
           (< (- (cadr actual) 1) size)) (generate_legal_moves actual
             (append posible_movs (list (list (+ (car actual) 2) (- (cadr actual) 1)))) size (+ step 1))) ;(2 -1)
       ((and (equal? step 6) (<= 0 (- (car actual) 2))
         (<= 0 (+ (cadr actual) 1)) (< (- (car actual) 2) size)
           (< (+ (cadr actual) 1) size)) (generate_legal_moves actual
             (append posible_movs (list (list (- (car actual) 2) (+ (cadr actual) 1)))) size (+ step 1))) ;(-2 1)
       ((and (equal? step 7) (<= 0 (- (car actual) 2))
         (<= 0 (- (cadr actual) 1)) (< (- (car actual) 2) size)
           (< (- (cadr actual) 1) size)) (generate_legal_moves actual
             (append posible_movs (list (list (- (car actual) 2) (- (cadr actual) 1)))) size (+ step 1)))( ;(-2 -1)
         else (generate_legal_moves actual posible_movs size (+ step 1))
         )
  ))

;Fucion que cuenta el largo de una lista
(define (lar lista res)(
  cond ((null? lista) res)(
    else (lar (cdr lista) (+ res 1))
    )
  ))

(define (get_last lista)(
  cond ((null? (cdr lista)) (car lista))(
    else (get_last (cdr lista))
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

;Validar si la poscion es valida
(define (checkPos matriz row col)(
  cond ((equal? (checkPos_AUX matriz row col) #t) #t )(
    else #f )
  ))

;Funcion Auxiliar para validar la posicion
(define (checkPos_AUX matriz row col)(
  cond ((and (equal? row 0) (equal? (get_ValAux col (car matriz)) 0)) #t)
       ((> row 0) (checkPos_AUX (cdr matriz) (- row 1) col))(
         else #f )
  ))

(define (clean_list lista matriz)(
  cond ((null? lista) '())(
    else (cleanAux lista matriz '())
    )
  ))

(define (cleanAux lista matriz res)(
  cond ((null? lista) res)
       ((equal? (checkPos matriz (caar lista) (cadar lista)) #t) (cleanAux (cdr lista) matriz (append res (list (car lista)))))(
    else (cleanAux (cdr lista) matriz res)
    )
  ))

(define (show_solution table)(
  write table
  ))

  (define (get_Empty_Neighbors legalMoves neighbors mat pos)
  (cond
    ((null? legalMoves) neighbors)
    ((equal? (checkPos mat (caar legalMoves) (cadar legalMoves)) #t) (get_Empty_Neighbors (cdr legalMoves) (append neighbors (list(car legalMoves))) mat pos))
    (else
     (get_Empty_Neighbors (cdr legalMoves) neighbors mat pos)
     )
    )
  )

(define (get_Scores neighbors scores matriz size)
  (cond
    ((null? neighbors) (car (sortScores scores '())))
    (else
     (get_Scores (cdr neighbors) (append scores (list(append (list(car neighbors)) (list(move_counter (car neighbors) matriz 0)))) ) matriz size)
     )
    )
  )
(define (move_counter pos matriz resultado)
  (lar (get_Empty_Neighbors (generate_legal_moves pos '() 8 0) '() matriz '()) 0)
  )
(define (sortScores scores res)
  (cond
    ((null? scores) res)
    ((null? res) (sortScores (cdr scores) (car scores)))
    ((< (cadar scores) (cadr res)) (sortScores (cdr scores) (car scores)))
    (else
     (sortScores (cdr scores) res)
     )
    )
  )

;Funcion que obtiene el tour del caballo
(define (cTour move path pos vecinos size table)(
  cond ((equal? move (* size size)) (show_solution path))
       ((equal? move 47) (show_solution path))
       ((null? (clean_list vecinos table)) (cTour (- move 1) (deLast path (lar path 0)) (get_last (deLast path (lar path 0))) (cdr (clean_list (generate_legal_moves (get_last (deLast path (lar path 0))) '() size 0) table)) size table))(
    else (cTour (+ move 1) (append path (list (car (clean_list vecinos table)))) (car vecinos) (clean_list (generate_legal_moves (car vecinos) '() size 0) table) size (change_Value table (car pos) (cadr pos) move))
    )
  ))

(define (PDC-Sol size pos)(
  cTour 1 '() pos (generate_legal_moves pos '() size 0) size (crear_Tablero size)
  ))

(get_Scores (get_Empty_Neighbors (generate_legal_moves '(0 0) '() 8 0 ) '() '((1 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0)) '(0 0)) '() '((1 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0)) 8)
;(PDC-Sol 8 '(0 0))
