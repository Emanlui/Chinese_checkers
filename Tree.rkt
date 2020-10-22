#lang racket/gui

(define matrix-of-tmp-pieces 0)
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

(define matrix-of-weights (list
                          '(0 0 1 2 2 10 11 11 11 11)
                          '(0 0 1 2 8  9 10 11 11 11)
                          '(1 1 1 2 7  8  9 10 11 11)
                          '(2 2 4 5 6  7  8  9 10 11)
                          '(2 2 3 4 5  6  7  8  9 10)
                          '(1 1 2 3 4  5  6  7  2  2)
                          '(0 1 1 2 3  4  5  2  2  2)
                          '(0 0 1 1 2  3  4  1  1  1)
                          '(0 0 0 1 1  2  3  1  0  0)
                          '(0 0 0 0 1  2  2  1  0  0)))

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

; This function choose the best play
; (choose-moving-tile 0 0)
(define (choose-moving-tile index best-play)
  (cond [(> index 9) (set! best-move-index best-play)]
    [(> (list-ref (list-ref list-of-tiles index) 0) (list-ref (list-ref list-of-tiles best-play) 0))
     (choose-moving-tile (+ index 1) index)]
    [else (choose-moving-tile (+ index 1) best-play)]))

; Verifies if the tile is in the finished zone
(define (verify-base-moves index)
  (cond [(<= index 9) (tile-has-finish index)(verify-base-moves (+ index 1)) ] ))

; Secondary function
(define (tile-has-finish index)
  (cond
    [(or (and (= 0 (second (list-ref list-of-tiles index))) (= 6 (third (list-ref list-of-tiles index))))
         (and (= 0 (second (list-ref list-of-tiles index))) (= 7 (third (list-ref list-of-tiles index))))
         (and (= 0 (second (list-ref list-of-tiles index))) (= 8 (third (list-ref list-of-tiles index))))
         (and (= 0 (second (list-ref list-of-tiles index))) (= 9 (third (list-ref list-of-tiles index))))
         (and (= 1 (second (list-ref list-of-tiles index))) (= 7 (third (list-ref list-of-tiles index))))
         (and (= 1 (second (list-ref list-of-tiles index))) (= 8 (third (list-ref list-of-tiles index))))
         (and (= 1 (second (list-ref list-of-tiles index))) (= 9 (third (list-ref list-of-tiles index))))
         (and (= 2 (second (list-ref list-of-tiles index))) (= 8 (third (list-ref list-of-tiles index))))
         (and (= 2 (second (list-ref list-of-tiles index))) (= 9 (third (list-ref list-of-tiles index))))
         (and (= 3 (second (list-ref list-of-tiles index))) (= 9 (third (list-ref list-of-tiles index))))
         )

     (set! list-of-tiles (list-set list-of-tiles index (list -1 (second (list-ref list-of-tiles index))(third (list-ref list-of-tiles index)))))]
   ))

; Adds 1 weight if the piece is not going to move
(define (add-weigth-list index)
  (cond [(or (> index 9) (= -1 (first (list-ref list-of-tiles index))))]
        [(= index best-move-index) (add-weigth-list (+ index 1))]
        [else (set! list-of-tiles (list-set list-of-tiles index (list (+ 1 (first (list-ref list-of-tiles index))) (second (list-ref list-of-tiles index))(third (list-ref list-of-tiles index)))))(add-weigth-list (+ index 1))]))

; This function validates the given position and also that the position is not the same type of piece
(define (validate-position x y)
  (cond [(or (< x 0) (< y 0) (> x 9) (> y 9)) #f]
        [else #t]))

(define (find-all-moves x y index)                                                            
;                                          |                            y , x-2           |   y+2   , x-2
;            | y , x-1 |  y+1   , x-1      |              |               1      |    1   |   
;  y-1   , x |     0   |  y+1   , x        | y-2   , x    |    1    |     0      |    1   |   y+2   , x
;  y-1 , x+1 | y , x+1 |                   |              |    1    |     1      |
; adds the current position to the list    | y-2   , x+2  |         |   y , x+2
  
  (if(and (validate-position y (- x 1))       (if (= 0 (list-ref (list-ref matrix-of-pieces (- x 1)) y)) #t #f))        (set! list-of-tmp-tiles (list-set list-of-tmp-tiles index (append (list-ref list-of-tmp-tiles index) (list (list (- x 1) y))))) void)  
  (if(and (validate-position (+ 1 y) (- x 1)) (if (= 0 (list-ref (list-ref matrix-of-pieces (- x 1)) (+ 1 y) )) #t #f)) (set! list-of-tmp-tiles (list-set list-of-tmp-tiles index (append (list-ref list-of-tmp-tiles index) (list (list (- x 1) (+ 1 y) ))))) void)   
  (if(and (validate-position (+ 1 y) x)       (if (= 0 (list-ref (list-ref matrix-of-pieces x) (+ 1 y))) #t #f))        (set! list-of-tmp-tiles (list-set list-of-tmp-tiles index (append (list-ref list-of-tmp-tiles index) (list (list x (+ 1 y)))))) void)  
  (if(and (validate-position y (+ 1 x))       (if (= 0 (list-ref (list-ref matrix-of-pieces (+ 1 x))y )) #t #f))        (set! list-of-tmp-tiles (list-set list-of-tmp-tiles index (append (list-ref list-of-tmp-tiles index) (list (list (+ 1 x) y))))) void)  
  (if(and (validate-position (- y 1) (+ 1 x)) (if (= 0 (list-ref (list-ref matrix-of-pieces (+ 1 x))(- y 1) )) #t #f))  (set! list-of-tmp-tiles (list-set list-of-tmp-tiles index (append (list-ref list-of-tmp-tiles index) (list (list (+ 1 x) (- y 1)))))) void)  
  (if(and (validate-position (- y 1) x)       (if (= 0 (list-ref (list-ref matrix-of-pieces x)(- y 1) )) #t #f))        (set! list-of-tmp-tiles (list-set list-of-tmp-tiles index (append (list-ref list-of-tmp-tiles index) (list (list x (- y 1)))))) void)  

  (set! matrix-of-tmp-pieces matrix-of-pieces)
  (find-all-moves-jump x y index)
 
     )
  
  
(define (find-all-moves-jump x y index)
;                                          |                            y , x-2           |   y+2   , x-2
;            | y , x-1 |  y+1   , x-1      |              |               1      |    1   |   
;  y-1   , x |     0   |  y+1   , x        | y-2   , x    |    1    |     0      |    1   |   y+2   , x
;  y-1 , x+1 | y , x+1 |                   |              |    1    |     1      |
; adds the current position to the list    | y-2   , x+2  |         |   y , x+2
  
  (if (and (validate-position (- x 2) y)       (or(= 2 (list-ref (list-ref matrix-of-tmp-pieces (- x 1)) y))(= 1 (list-ref (list-ref matrix-of-tmp-pieces (- x 1)) y)))              (= 0 (list-ref (list-ref matrix-of-tmp-pieces (- x 2)) y))) (jump-found x y (- x 2) y index)  void )
  (if (and (validate-position (- x 2) (+ y 2)) (or(= 2 (list-ref (list-ref matrix-of-tmp-pieces (- x 1)) (+ y 1))) (= 1 (list-ref (list-ref matrix-of-tmp-pieces (- x 1)) (+ y 1)))) (= 0 (list-ref (list-ref matrix-of-tmp-pieces (- x 2)) (+ y 2)))) (jump-found x y (- x 2) (+ y 2) index) void )
  (if (and (validate-position x (+ y 2))       (or(= 2 (list-ref (list-ref matrix-of-tmp-pieces x) (+ y 1)))(= 1 (list-ref (list-ref matrix-of-tmp-pieces x) (+ y 1))))              (= 0 (list-ref (list-ref matrix-of-tmp-pieces x) (+ y 2)))) (jump-found x y x (+ y 2) index) void )
  (if (and (validate-position (+ x 2) y)       (or(= 2 (list-ref (list-ref matrix-of-tmp-pieces (+ x 1)) y))(= 1 (list-ref (list-ref matrix-of-tmp-pieces (+ x 1)) y)))              (= 0 (list-ref (list-ref matrix-of-tmp-pieces (+ x 2)) y))) (jump-found x y (+ x 2) y index)  void )
  (if (and (validate-position (+ x 2) (- y 2)) (or(= 2 (list-ref (list-ref matrix-of-tmp-pieces (+ x 1)) (- y 1)))(= 1 (list-ref (list-ref matrix-of-tmp-pieces (+ x 1)) (- y 1))))  (= 0 (list-ref (list-ref matrix-of-tmp-pieces (+ x 2)) (- y 2)))) (jump-found x y (+ x 2) (- y 2) index) void )
  (if (and (validate-position x (- y 2))       (or(= 2 (list-ref (list-ref matrix-of-tmp-pieces x) (- y 1)))(= 1 (list-ref (list-ref matrix-of-tmp-pieces x) (- y 1))))              (= 0 (list-ref (list-ref matrix-of-tmp-pieces x) (- y 2)))) (jump-found x y x (- y 2) index) void )
  
  )

(define (jump-found x y future-x future-y index)
  (display (list x y future-x future-y index))
  (set! list-of-tmp-tiles (list-set list-of-tmp-tiles index (append (list-ref list-of-tmp-tiles index) (list (list future-x future-y)))))
  (set! matrix-of-pieces (list-set matrix-of-pieces x (list-set (list-ref matrix-of-pieces x) y -1)))
  (find-all-moves-jump future-x future-y index) 
  )

; This function set the weigth
(define (find-all-moves-function index)
  (cond[(> index 9)]
       [else (find-all-moves (second (list-ref list-of-tiles index)) (third (list-ref list-of-tiles index)) index) (find-all-moves-function (+ index 1)) ]))

(define (find-best-move index)
  (cond[(> index 9)]
       [else (set! list-of-tiles (list-set list-of-tiles index (list-set (list-ref list-of-tiles index) 0 (find-best-move-aux (list-ref list-of-tmp-tiles index) 0))))
        ;(displayln (list-ref list-of-tmp-tiles index) )
        (find-best-move (+ 1 index))]))


(define (find-best-move-aux lst weigth)
  (cond [(empty? lst) weigth]
  [(> (list-ref (list-ref matrix-of-weights (first (first lst))) (second(first lst))) weigth) (find-best-move-aux (rest lst) (list-ref (list-ref matrix-of-weights (first (first lst))) (second(first lst))))]
  [else (find-best-move-aux (rest lst) weigth)]
  ))

(define (run-AI)

  (find-all-moves-function 0)
  (find-best-move 0)
  ;(verify-base-moves 0)
  ;(choose-moving-tile 0 0)
  ;(displayln matrix-of-pieces)
  ;(displayln best-move-index)
  ;(set! list-of-tiles list-of-tmp-tiles)
  ;(add-weigth-list 0)
  (displayln list-of-tiles)
  (displayln "")
  (displayln list-of-tmp-tiles)
  )
