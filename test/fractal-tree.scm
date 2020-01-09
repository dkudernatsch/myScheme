(begin
  (define initLength 200)
  (define falloff-percent 80)
  (define iterations 10)
  (define angle 45)

  (define draw-tree
    (lambda (length depth curveature)
      (if (> depth 0)
        (begin
          (move-forward length)
          (turn-right curveature)
          (draw-tree (/ (* falloff-percent length) 100) (- depth 1) curveature)
          (turn-left (* curveature 2))
          (draw-tree (/ (* falloff-percent length) 100) (- depth 1) curveature)
          (turn-right curveature)
          (turn-left 180)
          (move-forward length)
          (turn-left 180)
        )
        nil
      )
    )
  )
  (draw-tree initLength iterations angle)
)