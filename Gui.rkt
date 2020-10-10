#lang racket/gui

(require racket/gui/base)
(require table-panel)

(define frame (new frame%
                 [label "Damas Chinas"]
                 ;[stretchable-width #f] [stretchable-height #f]
                 [height 700][width 700]
                ))

(define board
  (instantiate table-panel%
    (frame)
    (style '(border))
    (alignment '(center center))
    (dimensions '(9 9))))

(define first-click #f)
(define second-click #f)
(define first-position 0)
(define second-position 0)
(define first-tmp-color "")
(define second-tmp-color "")

(define wood "fotos/wood.png")
(define black "fotos/black.png")
(define red "fotos/red.png")
(define blue "fotos/blue.png")

(define list-of-image (list
        black black black black black blue blue blue blue
        black black black black black black blue blue blue
        black black black black  black black black blue blue
        black black black black black black black black blue
        black black black black  black black black black black
        red black black black   black black black black black
        red red black black black   black black black black
        red red red black black   black black black black
        red red red red black  black black black black))

(define grid-board
(for ((i (in-range 81)))
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
     [(false? first-click) (set! first-click #t) (set! second-click #f) (set! first-position i) (set! second-position 0)]
     [else (set! first-click #f)(set! second-click #t)(set! second-position i) (change-color) (set! first-position 0)] ))

; Change the color on the matrix
(define (change-color)
  (cond [(and (validate-move) (not(equal? first-position second-position)) (not(equal? (list-ref list-of-image first-position) black)))
  ; Sets the temp color of the buttons on variables
  (set! first-tmp-color (list-ref list-of-image first-position)) (set! second-tmp-color (list-ref list-of-image second-position))
  ; Sets the data on the matrix depending on the position
  (set! list-of-image(list-set list-of-image first-position second-tmp-color))(set! list-of-image (list-set list-of-image second-position first-tmp-color))
  ; Deletes the button
  (delete-button first-position) (delete-button second-position)
  ; Add new buttons
  (add-button first-position)  (add-button second-position)
  (winner)]))

; Delete a button on the board
(define (delete-button index)
  ;      This is the board panel in this exact position
  (send (list-ref (send board get-children) index)
        ; This is the delete option and the child, [board [panel position [first object or the button, there is only one]]]
        delete-child (list-ref (send (list-ref (send board get-children) index) get-children) 0)))

; Add a button in a position on the board
(define (add-button index)
  ;      This gets the panel object on the grid    create a new child     And this is the new child
  (send (list-ref (send board get-children) index) after-new-child (new button% [parent (list-ref (send board get-children) index)]
             [label (read-bitmap (list-ref list-of-image index))]
             [callback (lambda (button event) (button-click index (list-ref list-of-image index)))])))

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
        (not(false? ( or (equal? (list-ref list-of-image 45) blue)
                          (equal? (list-ref list-of-image 55) blue)
                          (equal? (list-ref list-of-image 54) blue)
                          (equal? (list-ref list-of-image 63) blue)
                          (equal? (list-ref list-of-image 64) blue)
                          (equal? (list-ref list-of-image 65) blue)
                          (equal? (list-ref list-of-image 72) blue)
                          (equal? (list-ref list-of-image 73) blue)
                          (equal? (list-ref list-of-image 74) blue)
                          (equal? (list-ref list-of-image 75) blue)))) #t]
       [else #f]))

; This function verifies if the red side won
(define (red-winner)
  (cond[
        (not(false? ( and (equal? (list-ref list-of-image 5) red)
                          (equal? (list-ref list-of-image 6) red)
                          (equal? (list-ref list-of-image 7) red)
                          (equal? (list-ref list-of-image 8) red)
                          (equal? (list-ref list-of-image 15) red)
                          (equal? (list-ref list-of-image 16) red)
                          (equal? (list-ref list-of-image 17) red)
                          (equal? (list-ref list-of-image 25) red)
                          (equal? (list-ref list-of-image 26) red)
                          (equal? (list-ref list-of-image 35) red)))) #t]
       [else #f]))

; This function validates the move
(define (validate-move)
  (cond [(or (equal? first-position (+ second-position 10))
             (equal? first-position (+ second-position 9))
             (equal? first-position (+ second-position 8))
             (equal? first-position (+ second-position 1))
             (equal? first-position (- second-position 1))
             (equal? first-position (- second-position 8))
             (equal? first-position (- second-position 9))
             (equal? first-position (- second-position 10))) #t]
       [else #f]))

; Show the frame to the screen
(send frame show #t)

;  0  1  2  3  4  5  6  7  8
;  9 10 11 12 13 14 15 16 17
; 18 19 20 21 22 23 24 25 26
; 27 28 29 30 31 32 33 34 35
; 36 37 38 39 40 41 42 43 44
; 45 46 47 48 49 50 51 52 53
; 54 55 56 57 58 59 60 61 62
; 63 64 65 66 67 68 69 70 71
; 72 73 74 75 76 77 78 79 80