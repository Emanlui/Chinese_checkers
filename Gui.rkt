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
    (dimensions '(13 13))))

(define first-click #f)
(define second-click #f)
(define first-position 0)
(define second-position 0)

(define wood "fotos/wood.png")
(define black "fotos/black.png")
(define red "fotos/red.png")
(define blue "fotos/blue.png")

(define list-of-image (list
        black black black black black black black black black blue blue blue blue
        black black black black black black black black black black blue blue blue
        black black black black black black black black black black black blue blue
        black black black black black black black black black black black black blue
        black black black black black black black black black black black black black
        black black black black black black black black black black black black black
        black black black black black black black black black black black black black
        black black black black black black black black black black black black black
        black black black black black black black black black black black black black
        red black black black black black black black black black black black black
        red red black black black black black black black black black black black
        red red red black black black black black black black black black black
        red red red red black black black black black black black black black))

(define bm (read-bitmap "fotos/base.jpg"))


(for ((i (in-range 169)))
  (let ((child (new panel%
             [parent table-panel]
             [style '(border)]
             [alignment '(center center)]
             )))
    (instantiate button%
        ((read-bitmap (list-ref list-of-image i) )
         child)  [callback (lambda (button event)
                         (button-click i))])
    ))

(define (button-click i)
    (cond
     [(false? first-click) (set! first-click #t) (set! second-click #f) (set! first-position i) (set! second-position 0)]
     [else (set! first-click #f)(set! second-click #t)(set! second-position i) (set! first-position 0)] ))

(send frame show #t)
