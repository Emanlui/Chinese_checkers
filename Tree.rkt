#lang racket/gui

(define matrix-of-pieces (list
                          '(0 0 0 0 0 0 1 1 1 1)
                          '(0 0 0 0 0 0 0 1 1 1)
                          '(0 0 0 0 0 0 0 0 1 1)
                          '(0 0 0 0 0 0 0 0 0 1)
                          '(0 0 0 0 0 0 0 0 0 0)
                          '(0 0 0 0 0 0 0 0 0 0)
                          '(2 0 0 0 0 0 0 0 0 0)
                          '(2 2 0 0 0 0 0 0 0 0)
                          '(2 2 2 0 0 0 0 0 0 0)
                          '(2 2 2 2 0 0 0 0 0 0)))

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
  (cond [(or (< x 1) (< y 1) (> x 9) (> y 9)) #f]
        [else #t]))

(define (find-all-moves x y index)

;            | x , y-1 |  x+1   , y-1
;  x-1   , y |     0   |  x+1   , y   
;  x-1 , y+1 | x , y+1 |
  ; adds the current position to the list
  
  (if(and (validate-position x (- 1 y))       (if (= 0 (list-ref (list-ref matrix-of-pieces (- 1 y)) x)) #t #f))        (set! list-of-tmp-tiles (list-set list-of-tmp-tiles index (list x (- 1 y)))) empty)  
  (if(and (validate-position (+ 1 x) (- 1 y)) (if (= 0 (list-ref (list-ref matrix-of-pieces (- 1 y)) (+ 1 x) )) #t #f)) (set! list-of-tmp-tiles (list-set list-of-tmp-tiles index (list (+ 1 x) (- 1 y)))) empty)   
  (if(and (validate-position (+ 1 x) y)       (if (= 0 (list-ref (list-ref matrix-of-pieces y) (+ 1 x))) #t #f))        (set! list-of-tmp-tiles (list-set list-of-tmp-tiles index (list (+ 1 x) y))) empty)  
  (if(and (validate-position x (+ 1 y))       (if (= 0 (list-ref (list-ref matrix-of-pieces (+ 1 y)) x)) #t #f))        (set! list-of-tmp-tiles (list-set list-of-tmp-tiles index (list x (+ 1 y)))) empty)  
  (if(and (validate-position (- 1 x) (+ 1 y)) (if (= 0 (list-ref (list-ref matrix-of-pieces (+ 1 y)) (- 1 x))) #t #f))  (set! list-of-tmp-tiles (list-set list-of-tmp-tiles index (list (- 1 x) (+ 1 y)))) empty)  
  (if(and (validate-position (- 1 x) y)       (if (= 0 (list-ref (list-ref matrix-of-pieces y) (- 1 x))) #t #f))        (set! list-of-tmp-tiles (list-set list-of-tmp-tiles index (list (- 1 x) y))) empty)  
        
     )
  

(define (find-all-moves-jump x y index)
  "A")

; This function set the weigth
(define (loop-for-the-best-move index)
  (cond[(> index 9)]
       [else (find-all-moves (first (list-ref list-of-tiles index)) (second (list-ref list-of-tiles index)) index)]))
      
(define (run-AI)
  
  (loop-for-the-best-move 0)
  ;(verify-base-moves 0)
  ;(choose-moving-tile 0 0)
  ;(displayln matrix-of-pieces)
  ;(displayln best-move-index)
  ;(set! list-of-tiles list-of-tmp-tiles)
  ;(add-weigth-list 0)
  (displayln list-of-tiles)
  (displayln list-of-tmp-tiles)
  )
