(begin
  (define initLength 100)
  (define iterations 12)

  (define draw-tree
    (lambda (length depth curveature)
      (if (> depth 0)
        (begin
          (move-forward length)
          (turn-right curveature)
          (draw-tree (/ (* 70 length) 100) (- depth 1) curveature)
          (turn-left (* curveature 2))
          (draw-tree (/ (* 70 length) 100) (- depth 1) curveature)
          (turn-right curveature)
          (turn-left 180)
          (move-forward length)
          (turn-left 180)
        )
        nil
      )
    )
  )
  (draw-tree initLength iterations 135)
)