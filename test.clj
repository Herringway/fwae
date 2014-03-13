(load "core")
(use 'parse-interp.core)

(defn run-test
	[program result testid]
	(if (not= (run program) result) (println "Test" (str testid) "Failed"))
); run-test
(defn run-failure-test
	[program testid]
	(try (run program) (println "Test" (str testid) "Failed") (catch Exception e))
); run-failure-test
(run-test '(4)             4 1)
(run-test '(+ 1 2)         3 2)
(run-test '(+ (- 4 4) 5)   5 3)
(run-test '(+ (- 4 4) 50) 50 4)
(run-test '(with ((x (+ 5 5))) (with ((y (- x 3))) (+ x y))) 17 5)
(run-test '(with ((x (+ 5 5))) (with ((x (- x 3))) (+ x x))) 14 6)
(run-test '(with ((x (+ 5 5)) (y (+ 3 3))) (+ x y))          16 7)
;(parsestr "{with {{x {+ 5 5}} {y {+ 3 3}}} {+ x y}}")
;(try (run '(with ((x (+ 5 5)) (x (+ 3 3))) (+ x x))) (println "Test 8 Failed") (catch Exception e))
(run-failure-test '(with ((x (+ 5 5)) (x (+ 3 3))) (+ x x)) 8)