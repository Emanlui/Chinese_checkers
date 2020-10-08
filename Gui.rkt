#lang racket/gui

(require racket/gui/base)
(require table-panel)

(define frame (new frame%
                 [label "Damas Chinas"]
                 ;[stretchable-width #f] [stretchable-height #f]
                 [height 700][width 700]
                ))
(define pos 1)

(define table-panel
  (instantiate table-panel%
    (frame)
    (style '(border))
    (alignment '(center center))
    (dimensions '(13 13))))

(for ((i (in-range 169)))
  (let ((child (instantiate table-panel%
                 (table-panel)
                 (style '(border))
                 (dimensions '(1 1))
                 )))
    (instantiate button%
        ((read-bitmap "fotos/blue.png") child))
    ))


(define button-list
  '("fotos/black.png" "fotos/blue.png" "fotos/red.png" "fotos/wood.png"))




(define bm (read-bitmap "fotos/base.jpg"))

(define mycanvas%
  (class canvas%
    (super-new)
    (inherit get-dc)
    (define/override (on-paint)
      (let ([my-dc (get-dc)])
        (send my-dc draw-bitmap bm pos pos)))))

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