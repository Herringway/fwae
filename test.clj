(load "simple_interp")
(use 'parse-interp.core)

(def simple-tests {
	'(4)             4, ;test simple numbers
	'(+ 1 2)         3, ;test addition
	'(- 3 1)         2, ;test subtraction
	'(+ (- 4 4) 5)   5, ;test nested operations
	'(+ 4 50)       54, ;test numbers greater than 9
	'(* 2 4)         8, ;test multiplication
	'(/ 10 5)        2, ;test division
	'(with ((x 3)) (+ x x))                             6,;test simple with
	'(with ((x (+ 5 5))) (with ((y (- x 3))) (+ x y))) 17,;test nested identifiers
	'(with ((x (+ 5 5))) (with ((x (- x 3))) (+ x x))) 14,;test nested and rebound identifiers
	'(with ((x (+ 5 5)) (y (+ 3 3))) (+ x y))          16 ;test multiple identifiers
}); simple-tests
(def simple-str-tests {  ;do we really even care?
	"{4}"             4, ;test simple numbers
	"(+ 1 2}"         3, ;test addition
	"{- 3 1}"         2, ;test subtraction
	"{+ {- 4 4} 5}"   5, ;test nested operations
	"{+ 4 50}"       54, ;test numbers greater than 9
	"{* 2 4}"         8, ;test multiplication
	"{/ 10 5}"        2, ;test division
	"{with {{x 3}} {+ x x}}"                             6,;test simple with
	"{with {{x {+ 5 5}}} {with {{y {- x 3}}} {+ x y}}}" 17,;test nested identifiers
	"{with {{x {+ 5 5}}} {with {{x {- x 3}}} {+ x x}}}" 14,;test nested and rebound identifiers
	"{with {{x {+ 5 5}} {y {+ 3 3}}} {+ x y}}"          16 ;test multiple identifiers
}); simple-str-tests

(def simple-failure-tests (list 
	'(+ 3),                                    ;addition test for insufficient args
	'(- 3),                                    ;subtraction test for insufficient args
	'(* 3),                                    ;multiplication test for insufficient args
	'(/ 3),                                    ;division test for insufficient args
	'(+ x 4),                                  ;addition test for unbound identifiers
	'(- x 4),                                  ;subtraction test for unbound identifiers
	'(* x 4),                                  ;multiplication test for unbound identifiers
	'(/ x 4),                                  ;division test for unbound identifiers
	'(with ((x (+ 5 5)) (x (+ 3 3))) (+ x x)), ;test for duplicate identifiers
	'(with ((1 (+ 5 5)) (x (+ 3 3))) (+ x x))  ;test for numeric identifiers
)); simple-failure-tests

(def interp-failure-tests (list 
	'(num x),    ;test invalid number
	'(unknown 2) ;test unknown operator
)); interp-failure-tests

(defn run-test
	"Test programs"
	[program result testid]
	(try 
		(if (not= (run program) result) 
			(str "Test " (str testid) " Failed") 
			(str "Test " (str testid) " Succeeded")) 
		(catch Exception e 
			(str "Test " (str testid) " Failed with message: " (.getMessage e))
		)
	)
); run-test
(defn run-str-test
	"Test programs in string form"
	[program result testid]
	(println "executing" program)
	(try 
		(if (not= (run-str program) result) 
			(str "Test " (str testid) " Failed") 
			(str "Test " (str testid) " Succeeded")) 
		(catch Exception e 
			(str "Test " (str testid) " Failed with message: " (.getMessage e))
		)
	)
); run-test
(defn run-failure-test
	"Run tests that are intended to produce errors"
	[program testid]
	(try (run program) 
		(str "Test " (str testid) " Failed") 
		(catch Exception e 
			(str "Test " (str testid) " Succeeded at generating an error: " (.getMessage e))
		)
	)
); run-failure-test
(defn interp-failure-test
	"Run interpreter tests that are intended to produce errors"
	[program testid]
	(try (interp program) 
		(str "Test " (str testid) " Failed") 
		(catch Exception e 
			(str "Test " (str testid) " Succeeded at generating an error: " (.getMessage e))
		)
	)
); run-failure-test
(defn run-tests
	"Runs all tests"
	[]
	(println (clojure.string/join "\n" (flatten (list
		(doall (map (fn [a b c] (run-test a b c)) (keys simple-tests) (vals simple-tests) (drop 1 (range))))
		(doall (map (fn [a b] (run-failure-test a b)) simple-failure-tests (drop (+ 1 (count simple-tests)) (range))))
		(doall (map (fn [a b] (interp-failure-test a b)) interp-failure-tests (drop (+ 1 (count simple-tests) (count simple-failure-tests)) (range))))
		(doall (map (fn [a b c] (run-str-test a b c)) (keys simple-str-tests) (vals simple-str-tests) (drop (+ 1 (count simple-tests) (count simple-failure-tests) (count interp-failure-tests)) (range))))
	))))
); run-tests
(run-tests)
