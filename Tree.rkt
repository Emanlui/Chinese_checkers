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


(define list-of-accumulated-weights (list 0 0 0 0 0 0 0 0 0 0))

; Stores the best play
(define best-move-index 0)
(define best-move-index-tmp 0)

; This is the list of pieces of the AI
;                   W X Y
;             (list 0 0 0)
(define list-of-tiles (list
                       (list 0 6 0) (list 0 7 0) (list 0 7 1) (list 0 8 0) (list 0 8 1) (list 0 8 2) (list 0 9 0)
                       (list 0 9 1) (list 0 9 2) (list 0 9 3)))

(define list-of-tmp-tiles (list
                       (list ) (list ) (list ) (list ) (list ) (list ) (list ) (list ) (list ) (list )))



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
  ;(display (list x y future-x future-y index))
  (set! list-of-tmp-tiles (list-set list-of-tmp-tiles index (append (list-ref list-of-tmp-tiles index) (list (list future-x future-y)))))
  (set! matrix-of-tmp-pieces (list-set matrix-of-pieces x (list-set (list-ref matrix-of-pieces x) y -1)))
  (find-all-moves-jump future-x future-y index) 
  )

; This function set the weigth
(define (find-all-moves-function index)
  (cond[(> index 9)]
       [else (find-all-moves (second (list-ref list-of-tiles index)) (third (list-ref list-of-tiles index)) index) (find-all-moves-function (+ index 1)) ]))

(define (find-best-move index)
  (cond[(> index 9)]
       [else
        ;(display "lista: ")
        ;(displayln (list-ref list-of-tmp-tiles index))
        (set! list-of-tiles (list-set list-of-tiles index (list-set (list-ref list-of-tiles index) 0 (find-best-move-aux (list-ref list-of-tmp-tiles index) (list 0 0 0)(list-ref list-of-accumulated-weights index)))))
        (find-best-move (+ 1 index))]))

(define (find-best-move-aux lst weigth accumulated-weigth)
  (cond [(empty? lst) weigth]
  [(< (first weigth) (find-max-weigth (first(first lst)) (second(first lst)) matrix-of-pieces 2 (+ accumulated-weigth (list-ref(list-ref matrix-of-weights (first(first lst)))(second(first lst))))))
   ;(display "NEW MAX WEIGTH: ")
   ;(displayln (find-max-weigth (first(first lst)) (second(first lst)) matrix-of-pieces 2 (+ accumulated-weigth (list-ref(list-ref matrix-of-weights (first(first lst)))(second(first lst))))))
   (find-best-move-aux (rest lst) (list (find-max-weigth (first(first lst)) (second(first lst)) matrix-of-pieces 2 (+ accumulated-weigth (list-ref(list-ref matrix-of-weights (first(first lst)))(second(first lst))))) (first(first lst))(second (first lst))) accumulated-weigth)]
  [else
   ;(display "Weigth: ")
   ;(displayln weigth)
   ;(display "Found WEIGTH: ")
   ;(displayln (list-ref(list-ref matrix-of-weights (first(first lst)))(second(first lst))))
   (find-best-move-aux (rest lst) weigth accumulated-weigth)]))

(define (find-max-weigth x y matrix level weigth)
  (cond [(= level 3) ;(display "Level:")    
                     ;(displayln level)
                     ;(display "Weigth:")    
                     ;(displayln weigth)
                     weigth]
        [else

   ;(display "Level:")    
   ;(displayln level)
   ;(display "Weigth:")    
   ;(displayln weigth)
   (set! matrix (list-set matrix x (list-set (list-ref matrix x) y 0)))
   
   (max 
  (if(and (validate-position y (- x 1))       (if (= 0 (list-ref (list-ref matrix (- x 1)) y)) #t #f))        (find-max-weigth (- x 1) y matrix (+ 1 level) (+ (list-ref(list-ref matrix-of-weights (- x 1))y) weigth)) 0)  
  (if(and (validate-position (+ 1 y) (- x 1)) (if (= 0 (list-ref (list-ref matrix (- x 1)) (+ 1 y) )) #t #f)) (find-max-weigth (- x 1) (+ 1 y) matrix (+ 1 level) (+ (list-ref(list-ref matrix-of-weights (- x 1))(+ 1 y)) weigth)) 0)   
  (if(and (validate-position (+ 1 y) x)       (if (= 0 (list-ref (list-ref matrix x) (+ 1 y))) #t #f))        (find-max-weigth x (+ 1 y) matrix (+ 1 level) (+ (list-ref(list-ref matrix-of-weights x)(+ 1 y)) weigth)) 0)  
  (if(and (validate-position y (+ 1 x))       (if (= 0 (list-ref (list-ref matrix (+ 1 x))y )) #t #f))        (find-max-weigth (+ 1 x) y matrix (+ 1 level) (+ (list-ref(list-ref matrix-of-weights (+ 1 x))y) weigth)) 0)  
  (if(and (validate-position (- y 1) (+ 1 x)) (if (= 0 (list-ref (list-ref matrix (+ 1 x))(- y 1) )) #t #f))  (find-max-weigth (+ 1 x) (- y 1) matrix (+ 1 level) (+ (list-ref(list-ref matrix-of-weights (+ 1 x))(- y 1)) weigth)) 0)  
  (if(and (validate-position (- y 1) x)       (if (= 0 (list-ref (list-ref matrix x)(- y 1) )) #t #f))        (find-max-weigth x (- y 1) matrix (+ 1 level) (+ (list-ref(list-ref matrix-of-weights x)(- y 1)) weigth)) 0)  

  (find-max-weigth-jump x y matrix (+ 1 level) weigth)
   )
   ]))

(define (find-max-weigth-jump x y matrix level weigth)
(cond [(= level 3) weigth]
        [else

   (set! matrix (list-set matrix x (list-set (list-ref matrix x) y 0)))

  (max (if (and (validate-position (- x 2) y)       (or(= 2 (list-ref (list-ref matrix (- x 1)) y))(= 1 (list-ref (list-ref matrix (- x 1)) y)))         (= 0 (list-ref (list-ref matrix (- x 2)) y))) (find-max-weigth-jump (- x 2) y matrix (+ 1 level) (+ (list-ref(list-ref matrix-of-weights (- x 2))y) weigth) )  0 )
  (if (and (validate-position (- x 2) (+ y 2)) (or(= 2 (list-ref (list-ref matrix (- x 1)) (+ y 1))) (= 1 (list-ref (list-ref matrix (- x 1)) (+ y 1)))) (= 0 (list-ref (list-ref matrix (- x 2)) (+ y 2)))) (find-max-weigth-jump  (- x 2) (+ y 2) matrix (+ 1 level) (+ (list-ref(list-ref matrix-of-weights (- x 2))(+ y 2)) weigth)) 0 )
  (if (and (validate-position x (+ y 2))       (or(= 2 (list-ref (list-ref matrix x) (+ y 1)))(= 1 (list-ref (list-ref matrix x) (+ y 1))))              (= 0 (list-ref (list-ref matrix x) (+ y 2)))) (find-max-weigth-jump  x (+ y 2) matrix (+ 1 level) (+ (list-ref(list-ref matrix-of-weights x)(+ y 2)) weigth)) 0 )
  (if (and (validate-position (+ x 2) y)       (or(= 2 (list-ref (list-ref matrix (+ x 1)) y))(= 1 (list-ref (list-ref matrix (+ x 1)) y)))              (= 0 (list-ref (list-ref matrix (+ x 2)) y))) (find-max-weigth-jump  (+ x 2) y matrix (+ 1 level) (+ (list-ref(list-ref matrix-of-weights (+ x 2))y) weigth))  0 )
  (if (and (validate-position (+ x 2) (- y 2)) (or(= 2 (list-ref (list-ref matrix (+ x 1)) (- y 1)))(= 1 (list-ref (list-ref matrix (+ x 1)) (- y 1))))  (= 0 (list-ref (list-ref matrix (+ x 2)) (- y 2)))) (find-max-weigth-jump  (+ x 2) (- y 2)matrix (+ 1 level) (+ (list-ref(list-ref matrix-of-weights (+ x 2))(- y 2)) weigth) ) 0 )
  (if (and (validate-position x (- y 2))       (or(= 2 (list-ref (list-ref matrix x) (- y 1)))(= 1 (list-ref (list-ref matrix x) (- y 1))))              (= 0 (list-ref (list-ref matrix x) (- y 2)))) (find-max-weigth-jump  x (- y 2) matrix (+ 1 level) (+ (list-ref(list-ref matrix-of-weights x)(- y 2)) weigth)) 0 )
  )])
  )

(define (add-one-to-not-best-move index)
  (cond[(> index 9) empty]
       [(= index best-move-index)(add-one-to-not-best-move (+ 1 index))]
       [else(set! list-of-accumulated-weights (list-set list-of-accumulated-weights index (+ 1 (list-ref list-of-accumulated-weights index))))(add-one-to-not-best-move (+ 1 index))])
  )

(define (get-best-move index)
  (cond[(> index 9) empty]
       [(> (first(first(list-ref list-of-tiles index))) (first(first(list-ref list-of-tiles best-move-index))))
        (set! best-move-index index) (get-best-move (+ 1 index))]
       [else(get-best-move (+ 1 index))]))

(define (verify-best-move)
(if (not(= best-move-index-tmp best-move-index))
      (set! list-of-accumulated-weights (list-set list-of-accumulated-weights best-move-index-tmp 0))empty)  )

(define (run-AI)

  (find-all-moves-function 0 )  
  (find-best-move 0)
  (get-best-move 0)
  (add-one-to-not-best-move 0)
  (verify-best-move)
  
  (set! best-move-index-tmp best-move-index)
  (set! matrix-of-pieces (list-set matrix-of-pieces (second(list-ref list-of-tiles best-move-index)) (list-set (list-ref matrix-of-pieces (second(list-ref list-of-tiles best-move-index))) (third(list-ref list-of-tiles best-move-index)) 0)))
  (set! matrix-of-pieces (list-set matrix-of-pieces (second(first(list-ref list-of-tiles best-move-index))) (list-set (list-ref matrix-of-pieces (second(first(list-ref list-of-tiles best-move-index)))) (third(first(list-ref list-of-tiles best-move-index))) 2)))

  
  ;(display "Weigth list: ")
  ;(displayln list-of-accumulated-weights)
  
  ;(displayln list-of-tmp-tiles)
  (set! list-of-tmp-tiles (list
                       (list ) (list ) (list ) (list ) (list ) (list ) (list ) (list ) (list ) (list )))
  ;(displayln "List of tiles")
  ;(displayln list-of-tiles)
  ;(displayln "List of tmp tiles")
  ;(displayln list-of-tmp-tiles)
  ;(set! list-of-tiles (list-set list-of-tiles best-move-index (list (first(list-ref list-of-tiles best-move-index)) (second(first(list-ref list-of-tiles best-move-index)))(third(first(list-ref list-of-tiles best-move-index))))))
 
  
  )
