(load "core")
(use 'parse-interp.core)
(-main '(+ (- 4 4) 5))
(-main '(with (x (+ 5 5)) (with (y (-x 3)) (+ y y))))
