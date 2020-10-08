 #lang racket/gui

(require racket/gui/base)

(define frame (new frame%
                 [label "Chinese checkers"]
                [height 700]
                [width 1400]))

; Add a horizontal panel to the dialog, with centering for buttons
(define panel (new horizontal-panel% [parent frame]
                                     [alignment '(center center)]))

(define vertical-panel (new vertical-panel% [parent frame]
                                     [alignment '(center center)]))

(define list-of-panels '(panel panel))


(define button-list
  ' (("fotos/black.png" "fotos/blue.png" "fotos/red.png")
     ("fotos/black.png" "fotos/blue.png" "fotos/red.png")))


(for ([i button-list])
     (for ([j i])
       (new button% [parent panel]
       [label (read-bitmap j)])))




(send frame show #t)


