#lang racket/gui

(require racket/gui/base)
(require table-panel)

(define frame (new frame%
                 [label "Damas Chinas"]
                 ;[stretchable-width #f] [stretchable-height #f]
                 [height 700][width 700]
                ))

(define table-panel
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

(define create-table
(for ((i (in-range 81)))
  (let ((child (new panel%
             [parent table-panel]
             [style '(border)]
             [alignment '(center center)]
             )))
    (new button% [parent (list-ref (send table-panel get-children) i)]
             [label (read-bitmap (list-ref list-of-image i))]
             [callback (lambda (button event) (button-click i (list-ref list-of-image i)))])
    )))

(define (button-click i color)
    (cond
     [(false? first-click) (set! first-click #t) (set! second-click #f) (set! first-position i) (set! second-position 0)]
     [else (set! first-click #f)(set! second-click #t)(set! second-position i) (change-color) (set! first-position 0)] ))

(define (change-color)
  (set! first-tmp-color (list-ref list-of-image first-position)) (set! second-tmp-color (list-ref list-of-image second-position))
  (list-set list-of-image first-position second-tmp-color)(list-set list-of-image second-position first-tmp-color)
  (delete-button first-position) (delete-button second-position)
  (add-button first-position)  (add-button second-position))

(define (delete-button index)
  (send (list-ref (send table-panel get-children) index)
        delete-child (list-ref (send (list-ref (send table-panel get-children) index) get-children) 0)))

(define (add-button index)
  (send (list-ref (send table-panel get-children) index) after-new-child (new button% [parent (list-ref (send table-panel get-children) index)]
             [label (read-bitmap (list-ref list-of-image index))]
             [callback (lambda (button event) (button-click index (list-ref list-of-image index)))])))
(send frame show #t)