(begin
  (define foldl
    (lambda (fn acc xs)
      (if (null? (head xs))
        acc
        (foldr fn (fn (head xs) acc) (tail xs))
      )))
  (define foldr
    (lambda (fn acc xs)
      (if (null? (head xs))
        acc
        (fn (head xs) (foldr fn acc (tail xs)))
      )))
  (define map
    (lambda (fn xs)
      (if (null? (head xs))
        nil
        (cons
          (fn (head xs))
          (map fn (tail xs))))))

  (define add1 (lambda (x) (+ x 1)))

  (define mark-begin
    (lambda (x)
      (begin
        (move-forward 20)
        (turn-right 180)
        (move-forward 20)
        (turn-right 180))))
  (define triangle-corner
    (lambda (length angle-offset)
      (begin
        (move-forward length)
        (turn-right (- 120 angle-offset))
        (move-forward length)
        (turn-right (- 120 angle-offset))
      )))
  (define foreach
    (lambda (range fn)
      (let (low (head range) high (head (tail range)))
          (if (< low high)
            (begin
              (fn low)
              (foreach (list (+ 1 low) high) fn)
            )
            (fn low)))
      ))
  (begin
    (turn-right 150)
    (foreach '(1 180)
      (lambda (i)
          (triangle-corner (- 180 i) 1)
        ))
  )
)
