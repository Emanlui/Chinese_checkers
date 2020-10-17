#lang racket/gui

;                   W X Y
(define piece (list 0 0 0))
(define piece1 (list 0 8 0))

; This is the list of pieces of the AI
(define list-of-tiles (list
                       piece piece piece piece piece piece piece piece piece piece1))

; This function choose the best play
(define (choose-moving-tile index best-play)
  (cond [(> index 9) best-play]
    [(> (list-ref (list-ref list-of-tiles index) 0) (list-ref (list-ref list-of-tiles best-play) 0))
     (choose-moving-tile (+ index 1) index)]
    [else (choose-moving-tile (+ index 1) best-play)]))

(define (verify-base-moves index)
  (cond [(<= index 9) (tile-has-finish index)(verify-base-moves (+ index 1)) ]
       ))

(define (tile-has-finish index)
  (cond
    [(or (= 6 (+(second (list-ref list-of-tiles index))(third (list-ref list-of-tiles index))) )
         (= 7 (+(second (list-ref list-of-tiles index))(third (list-ref list-of-tiles index))) )
         (= 8 (+(second (list-ref list-of-tiles index))(third (list-ref list-of-tiles index))) )
         (= 17 (+(second (list-ref list-of-tiles index))(third (list-ref list-of-tiles index))) )
         (= 18 (+(second (list-ref list-of-tiles index))(third (list-ref list-of-tiles index))) )
         (= 19 (+(second (list-ref list-of-tiles index))(third (list-ref list-of-tiles index))) )
         (= 28 (+(second (list-ref list-of-tiles index))(third (list-ref list-of-tiles index))) )
         (= 29 (+(second (list-ref list-of-tiles index))(third (list-ref list-of-tiles index))) )
         (= 39 (+(second (list-ref list-of-tiles index))(third (list-ref list-of-tiles index))) ))

     (set! list-of-tiles (list-set list-of-tiles index (list 2 (second (list-ref list-of-tiles index))(third (list-ref list-of-tiles index)))))]
   ))


;(define (calculate-weight list))

; (define (calculate X Y))
