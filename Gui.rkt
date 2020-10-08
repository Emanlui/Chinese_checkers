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
  (let ((child (instantiate table-panel%
                 (table-panel)
                 (style '(border))
                 (dimensions '(1 1))
                 )))
    (instantiate button%
        ((read-bitmap (list-ref list-of-image i)) child))
    ))

(define mycanvas%
  (class canvas%
    (super-new)
    (inherit get-dc)
    (define/override (on-paint)
      (let ([my-dc (get-dc)])
        (send my-dc draw-bitmap bm 1 1)))))

(define c (new mycanvas% [parent frame] ))


(send frame show #t)


;(for ([i button-list])
;     (for ([j i])
;       (cond
;         [(equal? (index-of button-list i) 0) (new button% [parent vertical-panel]
;                        [label (read-bitmap j)])]
;         [else (new button% [parent panel]
;                    [label (read-bitmap j)])]
;         )))