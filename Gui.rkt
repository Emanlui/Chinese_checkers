#lang racket/gui
(require racket/gui/base)
(require table-panel)

; Temporary matrix
(define matrix-of-tmp-pieces 0)

; Matrix of all the piece position
(define matrix-of-pieces (list
                          '(0 0 0 0 0 0 1 1 1 1)  ; 0
                          '(0 0 0 0 0 0 0 1 1 1)  ; 1
                          '(0 0 0 0 0 0 0 0 1 1)  ; 2
                          '(0 0 0 0 0 0 0 0 0 1)  ; 3
                          '(0 0 0 0 0 0 0 0 0 0)  ; 4
                          '(0 0 0 0 0 0 0 0 0 0)  ; 5 
                          '(2 0 0 0 0 0 0 0 0 0)  ; 6
                          '(2 2 0 0 0 0 0 0 0 0)  ; 7
                          '(2 2 2 0 0 0 0 0 0 0)  ; 8
                          '(2 2 2 2 0 0 0 0 0 0))); 9

; Matrix of weigths
(define matrix-of-weights (list
                          '(-inf.0 -inf.0 1 2 2 10 50 70 85 99)
                          '(-inf.0 -inf.0 1 2 8  9 10 50 70 80)
                          '(1 1 1 2 7  8 11 12 50 70)
                          '(1 2 4 5 6  7  8  9 10 50)
                          '(1 2 3 4 5  6  7  8  9 10)
                          '(1 1 2 3 4  5  6  7  2  2)
                          '(0 1 1 2 3  4  5  2  2  2)
                          '(0 0 1 1 2  3  4  1  1  1)
                          '(0 0 0 1 1  2  3  1  -inf.0 -inf.0)
                          '(0 0 0 0 1  1  1  1  -inf.0 -inf.0)))

; Stores the weigth if the piece is not moving
(define list-of-accumulated-weights (list 0 0 0 0 0 0 0 0 0 0))

; This is the list of valid moves
(define list-of-valid-moves (list ))

; Stores a value if a piece is in the end spot
(define list-of-ending-pieces (list 0 0 0 0 0 0 0 0 0 0 ))

; Stores the best play
(define best-move-index 0)

; This is the list of pieces of the AI
;                   W X Y
;             (list 0 0 0)
(define list-of-tiles (list
                       (list 0 6 0) (list 0 7 0) (list 0 7 1) (list 0 8 0) (list 0 8 1) (list 0 8 2) (list 0 9 0)
                       (list 0 9 1) (list 0 9 2) (list 0 9 3)))

(define list-of-tmp-tiles (list
                       (list ) (list ) (list ) (list ) (list ) (list ) (list ) (list ) (list ) (list )))


; Name:        validate-position
; Description: This function validates the given position and also that the position is not the same type of piece
; Input:
;         x = x position in the matrix
;         y = y position in the matrix
; Output: True if the position is valid in the matrix
;         False if the position is out of bounds
; Restrictions: None
(define (validate-position x y)
  (cond [(or (< x 0) (< y 0) (> x 9) (> y 9)) #f]
        [else #t]))

; Name:        validate-position
; Description: This function finds all moves with a coordinate
;                                          |                            y , x-2           |   y+2   , x-2
;            | y , x-1 |  y+1   , x-1      |              |               1      |    1   |   
;  y-1   , x |     0   |  y+1   , x        | y-2   , x    |    1    |     0      |    1   |   y+2   , x
;  y-1 , x+1 | y , x+1 |                   |              |    1    |     1      |
;                                          | y-2   , x+2  |         |   y , x+2
; Input:
;         x = x position in the matrix of the piece
;         y = y position in the matrix of the piece
; Output: None
; Restrictions: None
(define (find-all-moves x y index)                                                            
  
  (if(and (validate-position y (- x 1))       (if (= 0 (list-ref (list-ref matrix-of-pieces (- x 1)) y)) #t #f))        (set! list-of-tmp-tiles (list-set list-of-tmp-tiles index (append (list-ref list-of-tmp-tiles index) (list (list (- x 1) y))))) void)  
  (if(and (validate-position (+ 1 y) (- x 1)) (if (= 0 (list-ref (list-ref matrix-of-pieces (- x 1)) (+ 1 y) )) #t #f)) (set! list-of-tmp-tiles (list-set list-of-tmp-tiles index (append (list-ref list-of-tmp-tiles index) (list (list (- x 1) (+ 1 y) ))))) void)   
  (if(and (validate-position (+ 1 y) x)       (if (= 0 (list-ref (list-ref matrix-of-pieces x) (+ 1 y))) #t #f))        (set! list-of-tmp-tiles (list-set list-of-tmp-tiles index (append (list-ref list-of-tmp-tiles index) (list (list x (+ 1 y)))))) void)  
  (if(and (validate-position y (+ 1 x))       (if (= 0 (list-ref (list-ref matrix-of-pieces (+ 1 x))y )) #t #f))        (set! list-of-tmp-tiles (list-set list-of-tmp-tiles index (append (list-ref list-of-tmp-tiles index) (list (list (+ 1 x) y))))) void)  
  (if(and (validate-position (- y 1) (+ 1 x)) (if (= 0 (list-ref (list-ref matrix-of-pieces (+ 1 x))(- y 1) )) #t #f))  (set! list-of-tmp-tiles (list-set list-of-tmp-tiles index (append (list-ref list-of-tmp-tiles index) (list (list (+ 1 x) (- y 1)))))) void)  
  (if(and (validate-position (- y 1) x)       (if (= 0 (list-ref (list-ref matrix-of-pieces x)(- y 1) )) #t #f))        (set! list-of-tmp-tiles (list-set list-of-tmp-tiles index (append (list-ref list-of-tmp-tiles index) (list (list x (- y 1)))))) void)  

  (find-all-moves-jump x y index matrix-of-pieces)
     )
  
; Name:        find-all-moves-jump
; Description: This function finds all jump moves with a coordinate
;                                          |                            y , x-2           |   y+2   , x-2
;            | y , x-1 |  y+1   , x-1      |              |               1      |    1   |   
;  y-1   , x |     0   |  y+1   , x        | y-2   , x    |    1    |     0      |    1   |   y+2   , x
;  y-1 , x+1 | y , x+1 |                   |              |    1    |     1      |
;                                          | y-2   , x+2  |         |   y , x+2
; Input:
;         x = x position in the matrix of the piece
;         y = y position in the matrix of the piece
;         index = the index of the piece in the list-of-pieces
;         matrix = temp matrix for all the jumps
; Output: None
; Restrictions: None 
(define (find-all-moves-jump x y index matrix)
 
  (if (and (validate-position (- x 2) y)       (or(= 2 (list-ref (list-ref matrix (- x 1)) y))(= 1 (list-ref (list-ref matrix (- x 1)) y)))              (= 0 (list-ref (list-ref matrix (- x 2)) y))) (jump-found x y (- x 2) y index matrix)  void )
  (if (and (validate-position (- x 2) (+ y 2)) (or(= 2 (list-ref (list-ref matrix (- x 1)) (+ y 1))) (= 1 (list-ref (list-ref matrix (- x 1)) (+ y 1)))) (= 0 (list-ref (list-ref matrix (- x 2)) (+ y 2)))) (jump-found x y (- x 2) (+ y 2) index matrix) void )
  (if (and (validate-position x (+ y 2))       (or(= 2 (list-ref (list-ref matrix x) (+ y 1)))(= 1 (list-ref (list-ref matrix x) (+ y 1))))              (= 0 (list-ref (list-ref matrix x) (+ y 2)))) (jump-found x y x (+ y 2) index matrix) void )
  (if (and (validate-position (+ x 2) y)       (or(= 2 (list-ref (list-ref matrix (+ x 1)) y))(= 1 (list-ref (list-ref matrix (+ x 1)) y)))              (= 0 (list-ref (list-ref matrix (+ x 2)) y))) (jump-found x y (+ x 2) y index matrix)  void )
  (if (and (validate-position (+ x 2) (- y 2)) (or(= 2 (list-ref (list-ref matrix (+ x 1)) (- y 1)))(= 1 (list-ref (list-ref matrix (+ x 1)) (- y 1))))  (= 0 (list-ref (list-ref matrix (+ x 2)) (- y 2)))) (jump-found x y (+ x 2) (- y 2) index matrix) void )
  (if (and (validate-position x (- y 2))       (or(= 2 (list-ref (list-ref matrix x) (- y 1)))(= 1 (list-ref (list-ref matrix x) (- y 1))))              (= 0 (list-ref (list-ref matrix x) (- y 2)))) (jump-found x y x (- y 2) index matrix) void )
  
  )

; Name:         jump-found
; Description:  This function adds the valid jumps to the list-of-tmp-tiles, set the temp matrix to -1
;               to avoid infinite loops
; Input:   
;         x = The index to add in the X position
;         y = The index to add in the Y position
;         future-x = future-x position in the matrix of the piece
;         future-y = future-y position in the matrix of the piece
;         index = the index of the piece in the list-of-pieces
;         matrix = temp matrix for all the jumps
; Output: None
; Restrictions: None 
(define (jump-found x y future-x future-y index matrix)
  (set! list-of-tmp-tiles (list-set list-of-tmp-tiles index (append (list-ref list-of-tmp-tiles index) (list (list future-x future-y)))))
  (set! matrix (list-set matrix x (list-set (list-ref matrix x) y -1)))
  (find-all-moves-jump future-x future-y index matrix) 
  )

; Name:          jump-found
; Description:   This function loops the list of pieces to find all moves
; Input:   
;         index = The actual index
; Output: None
; Restrictions: None
(define (find-all-moves-function index)
  (cond[(> index 9)]
       [else (find-all-moves (second (list-ref list-of-tiles index)) (third (list-ref list-of-tiles index)) index) (find-all-moves-function (+ index 1)) ]))

; Name:          find-best-move
; Description:   This function loops the list of pieces to find all moves
; Input:   
;         index = The actual index
; Output: None
; Restrictions: None
(define (find-best-move index)
  (cond[(> index 9)]
       [else
        ;(display "lista: ")
        ;(displayln (list-ref list-of-tmp-tiles index))
        (set! list-of-tiles (list-set list-of-tiles index (list-set (list-ref list-of-tiles index) 0 (find-best-move-aux (list-ref list-of-tmp-tiles index) (list 0 0 0)(list-ref list-of-accumulated-weights index)))))
        (find-best-move (+ 1 index))]))

; Name:          find-best-move-aux
; Description:   This function finds the best move from all the valid moves
; Input:   
;         index = The actual index
;         lst = The list of moves
;         weigth = The best weigth
;         accumulated-weigth = Adds to the weigth the accumulated
; Output: None
; Restrictions: None
(define (find-best-move-aux lst weigth accumulated-weigth)
  (cond [(empty? lst) weigth]
  [(< (first weigth) (find-max-weigth (first(first lst)) (second(first lst)) matrix-of-pieces 2 (+ accumulated-weigth (list-ref(list-ref matrix-of-weights (first(first lst)))(second(first lst))))))
   (find-best-move-aux (rest lst) (list (find-max-weigth (first(first lst)) (second(first lst)) matrix-of-pieces 2 (+ accumulated-weigth (list-ref(list-ref matrix-of-weights (first(first lst)))(second(first lst))))) (first(first lst))(second (first lst))) accumulated-weigth)]
  [else
   (find-best-move-aux (rest lst) weigth accumulated-weigth)]))

; Name:          find-max-weigth
; Description:   This function finds the best move from all the valid moves
; Input:   
;         x  = the actual position in X
;         y  = the actual position in Y
;         matrix = The temp matrix of the actual level, it changes if there is a jump
;         level = The level rigth now, this is the tree level
;         weigth
; Output: None
; Restrictions: None
(define (find-max-weigth x y matrix level weigth)
  (cond [(= level 3)weigth]
        [else (set! matrix (list-set matrix x (list-set (list-ref matrix x) y 0)))
              (max 
              (if(and (validate-position y (- x 1))       (if (= 0 (list-ref (list-ref matrix (- x 1)) y)) #t #f))        (find-max-weigth (- x 1) y matrix (+ 1 level) (+ (list-ref(list-ref matrix-of-weights (- x 1))y) weigth)) 0)  
              (if(and (validate-position (+ 1 y) (- x 1)) (if (= 0 (list-ref (list-ref matrix (- x 1)) (+ 1 y) )) #t #f)) (find-max-weigth (- x 1) (+ 1 y) matrix (+ 1 level) (+ (list-ref(list-ref matrix-of-weights (- x 1))(+ 1 y)) weigth)) 0)   
              (if(and (validate-position (+ 1 y) x)       (if (= 0 (list-ref (list-ref matrix x) (+ 1 y))) #t #f))        (find-max-weigth x (+ 1 y) matrix (+ 1 level) (+ (list-ref(list-ref matrix-of-weights x)(+ 1 y)) weigth)) 0)  
              (if(and (validate-position y (+ 1 x))       (if (= 0 (list-ref (list-ref matrix (+ 1 x))y )) #t #f))        (find-max-weigth (+ 1 x) y matrix (+ 1 level) (+ (list-ref(list-ref matrix-of-weights (+ 1 x))y) weigth)) 0)  
              (if(and (validate-position (- y 1) (+ 1 x)) (if (= 0 (list-ref (list-ref matrix (+ 1 x))(- y 1) )) #t #f))  (find-max-weigth (+ 1 x) (- y 1) matrix (+ 1 level) (+ (list-ref(list-ref matrix-of-weights (+ 1 x))(- y 1)) weigth)) 0)  
              (if(and (validate-position (- y 1) x)       (if (= 0 (list-ref (list-ref matrix x)(- y 1) )) #t #f))        (find-max-weigth x (- y 1) matrix (+ 1 level) (+ (list-ref(list-ref matrix-of-weights x)(- y 1)) weigth)) 0)  

              (find-max-weigth-jump x y matrix (+ 1 level) weigth))
   ]))

; Name:          find-max-weigth-jump
; Description:   This function finds the max weigth for all the jumps
; Input:   
;         x  = the actual position in X
;         y  = the actual position in Y
;         matrix = The temp matrix of the actual level, it changes if there is a jump
;         level = The level rigth now, this is the tree level
;         weigth
; Output: None
; Restrictions: None
(define (find-max-weigth-jump x y matrix level weigth)
(cond [(= level 3) weigth]
        [else(set! matrix (list-set matrix x (list-set (list-ref matrix x) y 0)))

             (max (if (and (validate-position (- x 2) y)  (or(= 2 (list-ref (list-ref matrix (- x 1)) y))(= 1 (list-ref (list-ref matrix (- x 1)) y)))         (= 0 (list-ref (list-ref matrix (- x 2)) y))) (find-max-weigth-jump (- x 2) y matrix (+ 1 level) (+ 5 (list-ref(list-ref matrix-of-weights (- x 2))y) weigth) )  0 )
                  (if (and (validate-position (- x 2) (+ y 2)) (or(= 2 (list-ref (list-ref matrix (- x 1)) (+ y 1))) (= 1 (list-ref (list-ref matrix (- x 1)) (+ y 1)))) (= 0 (list-ref (list-ref matrix (- x 2)) (+ y 2)))) (find-max-weigth-jump  (- x 2) (+ y 2) matrix (+ 1 level) (+ 5 (list-ref(list-ref matrix-of-weights (- x 2))(+ y 2)) weigth)) 0 )
                  (if (and (validate-position x (+ y 2))       (or(= 2 (list-ref (list-ref matrix x) (+ y 1)))(= 1 (list-ref (list-ref matrix x) (+ y 1))))              (= 0 (list-ref (list-ref matrix x) (+ y 2)))) (find-max-weigth-jump  x (+ y 2) matrix (+ 1 level) (+ 5 (list-ref(list-ref matrix-of-weights x)(+ y 2)) weigth)) 0 )
                  (if (and (validate-position (+ x 2) y)       (or(= 2 (list-ref (list-ref matrix (+ x 1)) y))(= 1 (list-ref (list-ref matrix (+ x 1)) y)))              (= 0 (list-ref (list-ref matrix (+ x 2)) y))) (find-max-weigth-jump  (+ x 2) y matrix (+ 1 level) (+ 5 (list-ref(list-ref matrix-of-weights (+ x 2))y) weigth))  0 )
                  (if (and (validate-position (+ x 2) (- y 2)) (or(= 2 (list-ref (list-ref matrix (+ x 1)) (- y 1)))(= 1 (list-ref (list-ref matrix (+ x 1)) (- y 1))))  (= 0 (list-ref (list-ref matrix (+ x 2)) (- y 2)))) (find-max-weigth-jump  (+ x 2) (- y 2)matrix (+ 1 level) (+ 5 (list-ref(list-ref matrix-of-weights (+ x 2))(- y 2)) weigth) ) 0 )
                  (if (and (validate-position x (- y 2))       (or(= 2 (list-ref (list-ref matrix x) (- y 1)))(= 1 (list-ref (list-ref matrix x) (- y 1))))              (= 0 (list-ref (list-ref matrix x) (- y 2)))) (find-max-weigth-jump  x (- y 2) matrix (+ 1 level) (+ 5 (list-ref(list-ref matrix-of-weights x)(- y 2)) weigth)) 0 )
                  )]))

; Name:          add-one-to-not-best-move
; Description:   This function adds 1 to all not moving pieces
; Input:   
;         index  = The actual index of the list
; Output: None
; Restrictions: None
(define (add-one-to-not-best-move index)
  (cond[(> index 9) empty]
       [(= index best-move-index)(add-one-to-not-best-move (+ 1 index))]
       [else(set! list-of-accumulated-weights (list-set list-of-accumulated-weights index (+ 1 (list-ref list-of-accumulated-weights index))))(add-one-to-not-best-move (+ 1 index))]))

; Name:          get-best-move
; Description:   This function loops the list to find the best weigth
; Input:   
;         index  = The actual index of the list
; Output: None
; Restrictions: None
(define (get-best-move index)
  (cond[(> index 9) empty]
       [(> (first(first(list-ref list-of-tiles index))) (first(first(list-ref list-of-tiles best-move-index))))
        (set! best-move-index index) (get-best-move (+ 1 index))]
       [else(get-best-move (+ 1 index))]))

      
; Name:          get-valid-moves
; Description:   This function finds the max weigth for all the jumps
; Input:   
;         x  = the actual position in X
;         y  = the actual position in Y
; Output: None
; Restrictions: None
(define (get-valid-moves x y)
  (if(and (validate-position y (- x 1))       (if (= 0 (list-ref (list-ref matrix-of-pieces (- x 1)) y)) #t #f))        (set! list-of-valid-moves (append list-of-valid-moves (list(list (- x 1) y)))) void)  
  (if(and (validate-position (+ 1 y) (- x 1)) (if (= 0 (list-ref (list-ref matrix-of-pieces (- x 1)) (+ 1 y) )) #t #f)) (set! list-of-valid-moves (append list-of-valid-moves (list(list (- x 1) (+ 1 y) )))) void)   
  (if(and (validate-position (+ 1 y) x)       (if (= 0 (list-ref (list-ref matrix-of-pieces x) (+ 1 y))) #t #f))        (set! list-of-valid-moves (append list-of-valid-moves (list(list x (+ 1 y))))) void)  
  (if(and (validate-position y (+ 1 x))       (if (= 0 (list-ref (list-ref matrix-of-pieces (+ 1 x))y )) #t #f))        (set! list-of-valid-moves (append list-of-valid-moves (list(list (+ 1 x) y)))) void)  
  (if(and (validate-position (- y 1) (+ 1 x)) (if (= 0 (list-ref (list-ref matrix-of-pieces (+ 1 x))(- y 1) )) #t #f))  (set! list-of-valid-moves (append list-of-valid-moves (list(list (+ 1 x) (- y 1))))) void)  
  (if(and (validate-position (- y 1) x)       (if (= 0 (list-ref (list-ref matrix-of-pieces x)(- y 1) )) #t #f))        (set! list-of-valid-moves (append list-of-valid-moves (list(list x (- y 1))))) void)  

  (set! matrix-of-tmp-pieces matrix-of-pieces)
  (get-valid-moves-aux x y matrix-of-tmp-pieces #f))

; Name:          get-valid-moves-aux
; Description:   This function finds the max weigth for all the jumps
; Input:   
;         x  = the actual position in X
;         y  = the actual position in Y
;         matrix = This is the tmp matrix for this piece
;         add-move = If it is true, then adds it to the list of valid moves
; Output: None
; Restrictions: None
(define (get-valid-moves-aux x y matrix add-move)

  (set! matrix (list-set matrix x (list-set (list-ref matrix x) y -1)))
  (set! list-of-valid-moves (append list-of-valid-moves (list(list x y))))
  (if (and (validate-position (- x 2) y)       (or(= 2 (list-ref (list-ref matrix (- x 1)) y))(= 1 (list-ref (list-ref matrix (- x 1)) y)))              (= 0 (list-ref (list-ref matrix (- x 2)) y))) (get-valid-moves-aux (- x 2) y matrix #t)  void )
  (if (and (validate-position (- x 2) (+ y 2)) (or(= 2 (list-ref (list-ref matrix (- x 1)) (+ y 1))) (= 1 (list-ref (list-ref matrix (- x 1)) (+ y 1)))) (= 0 (list-ref (list-ref matrix (- x 2)) (+ y 2)))) (get-valid-moves-aux (- x 2) (+ y 2)matrix #t) void )
  (if (and (validate-position x (+ y 2))       (or(= 2 (list-ref (list-ref matrix x) (+ y 1)))(= 1 (list-ref (list-ref matrix x) (+ y 1))))              (= 0 (list-ref (list-ref matrix x) (+ y 2)))) (get-valid-moves-aux  x (+ y 2)matrix #t) void )
  (if (and (validate-position (+ x 2) y)       (or(= 2 (list-ref (list-ref matrix (+ x 1)) y))(= 1 (list-ref (list-ref matrix (+ x 1)) y)))              (= 0 (list-ref (list-ref matrix (+ x 2)) y))) (get-valid-moves-aux (+ x 2) y matrix #t)  void )
  (if (and (validate-position (+ x 2) (- y 2)) (or(= 2 (list-ref (list-ref matrix (+ x 1)) (- y 1)))(= 1 (list-ref (list-ref matrix (+ x 1)) (- y 1))))  (= 0 (list-ref (list-ref matrix (+ x 2)) (- y 2)))) (get-valid-moves-aux (+ x 2) (- y 2) matrix #t) void )
  (if (and (validate-position x (- y 2))       (or(= 2 (list-ref (list-ref matrix x) (- y 1)))(= 1 (list-ref (list-ref matrix x) (- y 1))))              (= 0 (list-ref (list-ref matrix x) (- y 2)))) (get-valid-moves-aux x (- y 2) matrix #t) void ))


; Name:          verify-ending-pieces
; Description:   This function verifies if the piece is at the end spot and then set the weigth to 0
; Input:   
;         index  = The index of the list
; Output: None
; Restrictions: None
(define (verify-ending-pieces index)
  (cond[(> index 9) empty]
       [(and (= (second(list-ref list-of-tiles index)) 0) (= (third(list-ref list-of-tiles index)) 6))
       
        (set! list-of-tiles (list-set list-of-tiles index (list-set (list-ref list-of-tiles index) 0 (list-set (first(list-ref list-of-tiles index)) 0 0 ))))
              (verify-ending-pieces (+ 1 index))]
       [(and (= (second(list-ref list-of-tiles index)) 0) (= (third(list-ref list-of-tiles index)) 7))
       
        (set! list-of-tiles(list-set list-of-tiles index (list-set (list-ref list-of-tiles index) 0 (list-set (first(list-ref list-of-tiles index)) 0 0 ))))
              (verify-ending-pieces (+ 1 index))]
       [(and (= (second(list-ref list-of-tiles index)) 0) (= (third(list-ref list-of-tiles index)) 8))
    
        (set! list-of-tiles(list-set list-of-tiles index (list-set (list-ref list-of-tiles index) 0 (list-set (first(list-ref list-of-tiles index)) 0 5 ))))
              (verify-ending-pieces (+ 1 index))]
       [(and (= (second(list-ref list-of-tiles index)) 0) (= (third(list-ref list-of-tiles index)) 9))
     
        (set! list-of-tiles(list-set list-of-tiles index (list-set (list-ref list-of-tiles index) 0 (list-set (first(list-ref list-of-tiles index)) 0 0 ))))
              (verify-ending-pieces (+ 1 index))]
       [(and (= (second(list-ref list-of-tiles index)) 1) (= (third(list-ref list-of-tiles index)) 7))
        
        (set! list-of-tiles(list-set list-of-tiles index (list-set (list-ref list-of-tiles index) 0 (list-set (first(list-ref list-of-tiles index)) 0 0 ))))
              (verify-ending-pieces (+ 1 index))]
       [(and (= (second(list-ref list-of-tiles index)) 1) (= (third(list-ref list-of-tiles index)) 8))
   
        (set! list-of-tiles(list-set list-of-tiles index (list-set (list-ref list-of-tiles index) 0 (list-set (first(list-ref list-of-tiles index)) 0 0 ))))
              (verify-ending-pieces (+ 1 index))]
       [(and (= (second(list-ref list-of-tiles index)) 1) (= (third(list-ref list-of-tiles index)) 9))
    
        (set! list-of-tiles(list-set list-of-tiles index (list-set (list-ref list-of-tiles index) 0 (list-set (first(list-ref list-of-tiles index)) 0 0 ))))
              (verify-ending-pieces (+ 1 index))]
       [(and (= (second(list-ref list-of-tiles index)) 2) (= (third(list-ref list-of-tiles index)) 8))
   
        (set! list-of-tiles(list-set list-of-tiles index (list-set (list-ref list-of-tiles index) 0 (list-set (first(list-ref list-of-tiles index)) 0 0 ))))
              (verify-ending-pieces (+ 1 index))]
       [(and (= (second(list-ref list-of-tiles index)) 2) (= (third(list-ref list-of-tiles index)) 9))
    
        (set! list-of-tiles(list-set list-of-tiles index (list-set (list-ref list-of-tiles index) 0 (list-set (first(list-ref list-of-tiles index)) 0 0 ))))
              (verify-ending-pieces (+ 1 index))]
       [(and (= (second(list-ref list-of-tiles index)) 3) (= (third(list-ref list-of-tiles index)) 9))
        
        (set! list-of-tiles(list-set list-of-tiles index (list-set (list-ref list-of-tiles index) 0 (list-set (first(list-ref list-of-tiles index)) 0 0 ))))
              (verify-ending-pieces (+ 1 index))]
       [else (verify-ending-pieces (+ 1 index))]))

;(define ms (current-inexact-milliseconds))

; Name:          run-AI
; Description:   This is the AI algorithm
; Input:  None
; Output: None
; Restrictions: None
(define (run-AI)
  ;(set! ms (current-inexact-milliseconds))

  (find-all-moves-function 0 )
  (find-best-move 0)
  (verify-ending-pieces 0)
  (get-best-move 0)
  (add-one-to-not-best-move 0)
  
  (set! list-of-accumulated-weights (list-set list-of-accumulated-weights best-move-index 0))
  (set! matrix-of-pieces (list-set matrix-of-pieces (second(list-ref list-of-tiles best-move-index)) (list-set (list-ref matrix-of-pieces (second(list-ref list-of-tiles best-move-index))) (third(list-ref list-of-tiles best-move-index)) 0)))
  (set! matrix-of-pieces (list-set matrix-of-pieces (second(first(list-ref list-of-tiles best-move-index))) (list-set (list-ref matrix-of-pieces (second(first(list-ref list-of-tiles best-move-index)))) (third(first(list-ref list-of-tiles best-move-index))) 2)))
  (set! list-of-tmp-tiles (list
                       (list ) (list ) (list ) (list ) (list ) (list ) (list ) (list ) (list ) (list )))

  ;(displayln (- (current-inexact-milliseconds) ms))
  )


; Name:          print-matrix
; Description:   This funtion prints the matrix
; Input:  None
; Output: None
; Restrictions: None
(define (print-matrix matrix-of-pieces-tmp)
  (cond [( empty? matrix-of-pieces-tmp) empty]
        [else (displayln (first matrix-of-pieces-tmp))
  (print-matrix (rest matrix-of-pieces-tmp))]))

;----------------------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------------------
; The separation from the logic to the GUI
;----------------------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------------------

(define (do-best-move current-position next-position);currentpos and nextpos are both a single nomber (refering list-of-image)
  (cond
    [(equal? first-click #f)
     (set! first-click #f)
     (set! second-click #t)
     (set! first-position current-position)
     (set! second-position next-position)
     (change-color)]
    [else empty]))

;Get a pair and returns a single index for list logic
(define (to-single-index lst)
  (+ (* (first lst) 10) (second lst))
  )

;This function returns a list
;having two single indexes (current-place and next-place),
;something like (65 73)
(define (get-swaping-indexes)
  (get-swaping-aux 0 0))

(define (get-swaping-aux index max-weight-index)
  ;(displayln list-of-tiles)
  ;(displayln "coordinates") (displayln (list (second (list-ref list-of-tiles max-weight-index)) (third (list-ref list-of-tiles max-weight-index))))
  (cond
    [(> index (- (length list-of-tiles) 1)) (list (to-single-index (list (second (list-ref list-of-tiles max-weight-index)) (third (list-ref list-of-tiles max-weight-index))))
                                            (to-single-index (list (second (first (list-ref list-of-tiles max-weight-index))) (third (first (list-ref list-of-tiles max-weight-index))))))]
    [(> (first (first (list-ref list-of-tiles index))) (first (first (list-ref list-of-tiles max-weight-index)))) (get-swaping-aux (+ index 1) index)]
    [else (get-swaping-aux (+ 1 index) max-weight-index)]
    )
  )

(define (do-IA)
  (run-AI)
  (do-best-move (first (get-swaping-indexes)) (second (get-swaping-indexes)))
  (set! list-of-tiles (list-set list-of-tiles best-move-index (list (first(list-ref list-of-tiles best-move-index)) (second(first(list-ref list-of-tiles best-move-index)))(third(first(list-ref list-of-tiles best-move-index))))))
  )

(define frame (new frame%
                 [label "Chinese checkers"]
                 ;[stretchable-width #f] [stretchable-height #f]
                 [height 700][width 700]
                ))

(define board
  (instantiate table-panel%
    (frame)
    (style '(border))
    (alignment '(center center))
    (dimensions '(10 10))))

(define message-turn (new message%
  [parent frame]
  [stretchable-width #t]
  [label "Now its the turn for: "]
  ))

(define first-click #f)
(define second-click #f)
(define first-position 0)
(define second-position 0)
(define first-tmp-color "")
(define second-tmp-color "")
(define last-move-color "")
(define tmp-logical-value 0)

(define wood "fotos/wood.png")
(define black "fotos/black.png")
(define red "fotos/red.png")
(define blue "fotos/blue.png")

(define list-of-image (list
        black black black black black black blue blue blue blue
        black black black black black black black blue blue blue
        black black black black black black black black blue blue
        black black black black black black black black black blue
        black black black black black black black black black black
        black black black black black black black black black black
        red black black black black black black black black black
        red red black black black black black black black black
        red red red black black black black black black black
        red red red red black black black black black black))


(define grid-board
(for ((i (in-range 100)))
  (let ((child (new panel%
             [parent board]
             [style '(border)]
             [alignment '(center center)]
             )))
    (new button% [parent (list-ref (send board get-children) i)]
             [label (read-bitmap (list-ref list-of-image i))]
             [callback (lambda (button event) (button-click i (list-ref list-of-image i)))])
    )))

(define (button-click i color)
    (cond
      [(equal? last-move-color blue) empty]
     [(false? first-click) (set! first-click #t) (set! second-click #f) (set! first-position i) (set! second-position 0)]
     [else (set! first-click #f)(set! second-click #t)(set! second-position i) (get-valid-moves (quotient first-position 10) (remainder first-position 10))  (change-color) (set! list-of-valid-moves (list )) (set! first-position 0) (if (equal? last-move-color blue) (do-IA) empty)]
     ))

(define (validate-blue-move lst position)
   (cond
      [(empty? lst) #f]
      [(equal? (first lst) (list (quotient position 10) (remainder position 10)) )  #t]
      [else (validate-blue-move (rest lst) position)]))

; Change the color on the matrix
(define (change-color)
  (cond [(and (not(equal? last-move-color (list-ref list-of-image first-position))) (not(equal? first-position second-position)) (not(equal? (list-ref list-of-image first-position) black)) (or (equal? (list-ref list-of-image first-position) red)(validate-blue-move list-of-valid-moves second-position)))
  ; Change the color label
  (change-color-turn)
  ; Sets the new "last-move-color", this variable stores the last button color that was move
  (set! last-move-color (list-ref list-of-image first-position))
  ; Sets the temp color of the buttons on variables
  (set! first-tmp-color (list-ref list-of-image first-position)) (set! second-tmp-color (list-ref list-of-image second-position))
  ; Sets the data on the matrix depending on the position
  (set! list-of-image(list-set list-of-image first-position second-tmp-color))(set! list-of-image (list-set list-of-image second-position first-tmp-color))
  ; Changes the board on the logical matrix

  ; Deletes the button
  (delete-button first-position) (delete-button second-position)
  ; Add new buttons
  (add-button first-position) (add-button second-position)
  (update-mtx)
  ;(displayln list-of-image)
  (winner)
  
  ]))



; Delete a button on the board
(define (delete-button index)
  ;      This is the board panel in this exact position
  (send (list-ref (send board get-children) index)
        ; This is the delete option and the child, [board [panel position [first object or the button, there is only one]]]
        delete-child (list-ref (send (list-ref (send board get-children) index) get-children) 0))
  )

; Add a button in a position on the board
(define (add-button index)
  ;      This gets the panel object on the grid    create a new child     And this is the new child
  (send (list-ref (send board get-children) index) after-new-child (new button% [parent (list-ref (send board get-children) index)]
             [label (read-bitmap (list-ref list-of-image index))]
             [callback (lambda (button event) (button-click index (list-ref list-of-image index)))]))
  
  
  )

; This function verifies if there is a winner, calling blue-winner and red-winner, if it returns true, then it popups a windows
; and also disable all buttons
(define (winner)
  (cond [(not(false? (blue-winner))) (disable-buttons) (define dialog (new dialog% [label "Winner winner chicken dinner"][height 200][width 200]))
  (define msg (new message% [parent dialog]
                          [label "Blue player wins"]))(send dialog show #t)  ]
        [(not(false? (red-winner))) (disable-buttons) (define dialog (new dialog% [label "Winner winner chicken dinner"][height 200][width 200]))
  (define msg (new message% [parent dialog]
                          [label "Red player wins"]))(send dialog show #t)  ]))

; This function disable all buttons
(define (disable-buttons)
  (for ([j (send board get-children)]) ; iterator binding
  (send (list-ref (send j get-children) 0) enable #f)))

; This function verifies if the blue side won
(define (blue-winner)
  (cond[
        (not(false? ( and (equal? (list-ref list-of-image 60) blue)
                          (equal? (list-ref list-of-image 70) blue)
                          (equal? (list-ref list-of-image 71) blue)
                          (equal? (list-ref list-of-image 80) blue)
                          (equal? (list-ref list-of-image 81) blue)
                          (equal? (list-ref list-of-image 82) blue)
                          (equal? (list-ref list-of-image 90) blue)
                          (equal? (list-ref list-of-image 91) blue)
                          (equal? (list-ref list-of-image 92) blue)
                          (equal? (list-ref list-of-image 93) blue)))) #t]
       [else #f]))

; This function verifies if the red side won
(define (red-winner)
  (cond[
        (not(false? ( and (equal? (list-ref list-of-image 6) red)
                          (equal? (list-ref list-of-image 7) red)
                          (equal? (list-ref list-of-image 8) red)
                          (equal? (list-ref list-of-image 9) red)
                          (equal? (list-ref list-of-image 17) red)
                          (equal? (list-ref list-of-image 18) red)
                          (equal? (list-ref list-of-image 19) red)
                          (equal? (list-ref list-of-image 28) red)
                          (equal? (list-ref list-of-image 29) red)
                          (equal? (list-ref list-of-image 39) red)))) #t]
       [else #f]))

; This function validates the move
(define (validate-move x y)
  (cond
    [(equal? last-move-color blue) #t]
    [(or 
             (equal? x (+ y 10))
             (and (equal? x (+ y 9)) (not (equal? (modulo y 10) 0) ) );pasar de 39 a 30
             (and (equal? x (+ y 1)) (not (equal? (modulo x 10) 0) ) ); pasar de 39 a 40
             (and(equal? x (- y 1)) (not (equal? (modulo y 10) 0) ) ); pasar de 40 a 39
             (and (equal? x (- y 9)) (not (equal? (modulo x 10) 0) ) );pasar de 30 a 39
             (equal? x (- y 10))
             ) #t]
       [else #f])); (not (equal? (- y (- y -9)) 9))

; This function changes the label turn
(define (change-color-turn)
  (cond [(equal? last-move-color blue) (send message-turn set-label "Now its the turn for: BLUE")]
        [(equal? last-move-color red) (send message-turn set-label "Now its the turn for: RED")]))

; Show the frame to the screen
(send frame show #t)


;This function updates matrix of pieces
;by using list-of-image data
(define (update-mtx)
  (update-mtx-aux 0)
  )

(define (update-mtx-aux index)
  (cond
    [(> index (- (length list-of-image) 1)) empty]
    [(equal? (list-ref list-of-image index) blue) (set! matrix-of-pieces (list-set matrix-of-pieces (quotient index 10) (list-set (list-ref matrix-of-pieces (quotient index 10)) (modulo index 10) 1)))
                                                  (update-mtx-aux (+ index 1))]
    [(equal? (list-ref list-of-image index) red) (set! matrix-of-pieces (list-set matrix-of-pieces (quotient index 10) (list-set (list-ref matrix-of-pieces (quotient index 10)) (modulo index 10) 2)))
                                                 (update-mtx-aux (+ index 1))]
    [(equal? (list-ref list-of-image index) black) (set! matrix-of-pieces (list-set matrix-of-pieces (quotient index 10) (list-set (list-ref matrix-of-pieces (quotient index 10)) (modulo index 10) 0)))
                                                   (update-mtx-aux (+ index 1))]
    )
  )

;(define (printmtx mtx)
;  (displayln "La matriz: ")
;  (for ([i mtx])
;    (displayln i))
;  )

;  0  1  2  3  4  5  6  7  8  9
; 10 11 12 13 14 15 16 17 18 19
; 20 21 22 23 24 25 26 27 28 29
; 30 31 32 33 34 35 36 37 38 39
; 40 41 42 43 44 45 46 47 48 49
; 50 51 52 53 54 55 56 57 58 59
; 60 61 62 63 64 65 66 67 68 69
; 70 71 72 73 74 75 76 77 78 79
; 80 81 82 83 84 85 86 87 88 89
; 90 91 92 93 94 95 96 97 98 99