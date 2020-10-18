#lang racket/gui

(define matrix-of-pieces (list
                          '(0 0 0 0 0 0 1 1 1 1)
                          '(0 0 0 0 0 0 0 1 1 1)
                          '(0 0 0 0 0 0 0 0 1 1)
                          '(0 0 0 0 0 0 0 0 0 1)
                          '(0 0 0 0 0 0 0 0 0 0)
                          '(0 1 0 0 0 0 0 0 0 0)
                          '(2 0 0 0 0 0 0 0 0 0)
                          '(2 2 0 0 0 0 0 0 0 0)
                          '(2 2 2 0 0 0 0 0 0 0)
                          '(2 2 2 2 0 0 0 0 0 0)))

; Stores the best play
(define best-move-index 0)

; This is the list of pieces of the AI
;                   W X Y
;             (list 0 0 0)
(define list-of-tiles (list
                       (list 0 6 0) (list 0 7 0) (list 0 7 1) (list 0 8 0) (list 0 8 1) (list 0 8 2) (list 0 9 0)
                       (list 0 9 1) (list 0 9 2) (list 0 9 3)))
(define list-of-tmp-tiles (list
                       (list 0 0 0) (list 0 0 0) (list 0 0 0) (list 0 0 0) (list 0 0 0) (list 0 0 0) (list 0 0 0)
                       (list 0 0 0) (list 0 0 0) (list 0 0 0)))

; This function choose the best play
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
  (cond [(or (< x 1) (< y 1) (> x 8) (> y 8) (= 2 (list-ref (list-ref matrix-of-pieces x) y))) #f]
        [else #t]))

; This function calculates de weigth of a given piece
(define (calculate-weight w x y)
  ; Obtains the best move
        ;   first we validate the move          if it is valid the checks if the move has a 0 (free space) and if it is a 0 then returns w+1
  (max (if (validate-position (+ x 1) (+ y 1)) (if (= 0 (list-ref (list-ref matrix-of-pieces (+ x 1)) (+ y 1))) (+ w 1)
           ; if not, then validate the jump, if it is a valid jump, then we call our secondary function for jumps, if it is not valid the returns 0
           (if (and (validate-position (+ x 2)(+ y 2)) (= 0 (list-ref (list-ref matrix-of-pieces (+ x 2)) (+ y 2)))) (calculate-weight-jump (+ w 7) (+ x 2) (+ y 2)) w) ) 0)
       
       (if (validate-position (- x 1) (+ y 1)) (if (= 0 (list-ref (list-ref matrix-of-pieces (- x 1)) (+ y 1))) (+ w 1)
           (if (and (validate-position (- x 2)(+ y 2)) (= 0 (list-ref (list-ref matrix-of-pieces (- x 2)) (+ y 2)))) (calculate-weight-jump (+ w 7) (- x 2) (+ y 2)) w) ) 0)
       
       (if (validate-position (- x 1) (- y 1)) (if (= 0 (list-ref (list-ref matrix-of-pieces (- x 1)) (- y 1))) (+ w 1)
           (if (and (validate-position (- x 2)(- y 2)) (= 0 (list-ref (list-ref matrix-of-pieces (- x 2)) (- y 2)))) (calculate-weight-jump (+ w 7) (- x 2) (- y 2)) w) ) 0)
       
       (if (validate-position (+ x 1) (- y 1)) (if (= 0 (list-ref (list-ref matrix-of-pieces (+ x 1)) (- y 1))) (+ w 1)
           (if (and (validate-position (+ x 2)(- y 2)) (= 0 (list-ref (list-ref matrix-of-pieces (+ x 2)) (- y 2)))) (calculate-weight-jump (+ w 7) (+ x 2) (- y 2)) w) ) 0)))

; This function only iterates jumps on the matrix
(define (calculate-weight-jump w x y)
  (set! matrix-of-pieces (list-set matrix-of-pieces x (list-set (list-ref matrix-of-pieces x) y -1)))
  (max (if (validate-position (+ x 2)(+ y 2)) (if (and (= 1 (list-ref (list-ref matrix-of-pieces (+ x 1)) (+ y 1))) (= 0 (list-ref (list-ref matrix-of-pieces (+ x 2)) (+ y 2)))) (calculate-weight-jump (+ w 7) (+ x 2) (+ y 2)) w) w)
       (if (validate-position (+ x 2)(- y 2)) (if (and (= 1 (list-ref (list-ref matrix-of-pieces (+ x 1)) (- y 1))) (= 0 (list-ref (list-ref matrix-of-pieces (+ x 2)) (- y 2)))) (calculate-weight-jump (+ w 7) (+ x 2) (- y 2)) w) w)
       (if (validate-position (- x 2)(+ y 2)) (if (and (= 1 (list-ref (list-ref matrix-of-pieces (- x 1)) (+ y 1))) (= 0 (list-ref (list-ref matrix-of-pieces (- x 2)) (+ y 2)))) (calculate-weight-jump (+ w 7) (- x 2) (+ y 2)) w) w)
       (if (validate-position (- x 2)(- y 2)) (if (and (= 1 (list-ref (list-ref matrix-of-pieces (- x 1)) (- y 1))) (= 0 (list-ref (list-ref matrix-of-pieces (- x 2)) (- y 2)))) (calculate-weight-jump (+ w 7) (- x 2) (- y 2)) w) w)
   ))



; This function set the weigth
(define (loop-for-the-best-move index)
  (cond[(> index 9)]
       [else (set! list-of-tiles (list-set list-of-tiles index (list-set (list-ref list-of-tiles index) 0 (calculate-weight (first (list-ref list-of-tiles index))
                          (second (list-ref list-of-tiles index))
                          (third (list-ref list-of-tiles index)))))) (clean-matrix) (loop-for-the-best-move (+ 1 index))]))

; This function cleans the matrix that has -1
(define (clean-matrix)
  (for ((x (in-range 10)) 
  )(for ((y (in-range 10)) 
  )(if (= -1 (list-ref(list-ref matrix-of-pieces x)y))
       (set! matrix-of-pieces (list-set matrix-of-pieces x (list-set (list-ref matrix-of-pieces x) y 0))) null))))

;(define (move-piece-to-position)
;  (set! matrix-of-pieces (list-set matrix-of-pieces (first (list-ref list-of-tiles best-move-index))
;                                   (list-set (list-ref matrix-of-pieces (first (list-ref list-of-tiles best-move-index)))
;                                             (second (list-ref list-of-tiles best-move-index)) 2)))
;  )

(define (run-AI)
  (set! list-of-tmp-tiles list-of-tiles)
  (loop-for-the-best-move 0)
  (verify-base-moves 0)
  (choose-moving-tile 0 0)
  (display matrix-of-pieces)
  (display best-move-index)
  (set! list-of-tiles list-of-tmp-tiles)
  (add-weigth-list 0)
  (display list-of-tiles)
  (move-piece-to-position))
