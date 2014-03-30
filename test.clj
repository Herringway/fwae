(load "xinterp")
(load "xinterp_lazy")

;Tests by Cameron Ross, 2014

(def simple-tests {
  '(4)             4, ;test simple numbers
  "{4}"            4, ;test simple numbers
  '(+ 1 2)         3, ;test addition
  "(+ 1 2}"        3, ;test addition
  '(- 3 1)         2, ;test subtraction
  "{- 3 1}"        2, ;test subtraction
  '(+ (- 4 4) 5)   5, ;test nested operations
  "{+ {- 4 4} 5}"  5, ;test nested operations
  '(+ 4 50)       54, ;test numbers greater than 9
  "{+ 4 50}"      54, ;test numbers greater than 9
  '(* 2 4)         8, ;test multiplication
  "{* 2 4}"        8, ;test multiplication
  '(/ 10 5)        2, ;test division
  "{/ 10 5}"       2, ;test division
  '(with ((x 3)) (+ x x))                             6,;test simple with
  "{with {{x 3}} {+ x x}}"                            6,;test simple with
  '(with ((x (+ 5 5))) (with ((y (- x 3))) (+ x y))) 17,;test nested identifiers
  "{with {{x {+ 5 5}}} {with {{y {- x 3}}} {+ x y}}}"17,;test nested identifiers
  '(with ((x (+ 5 5))) (with ((x (- x 3))) (+ x x))) 14,;test nested and rebound identifiers
  "{with {{x {+ 5 5}}} {with {{x {- x 3}}} {+ x x}}}"14,;test nested and rebound identifiers
  '(with ((x (+ 5 5)) (y (+ 3 3))) (+ x y))          16,;test multiple identifiers
  "{with {{x {+ 5 5}} {y {+ 3 3}}} {+ x y}}"         16,;test multiple identifiers
  '((fun () (* 2 4)))                                 8,;test zero arg function
  "{{fun {} {* 2 4}}}"                                8,;test zero arg function
  '(with ((x 2) (y 3)) (with ((z (+ x y))) (+ x z)))  7,;example given in the assignment
  "{with {{x 2} {y 3}} {with {{z {+ x y}}} {+ x z}}}" 7,;example given in the assignment
  '((fun (x) (* x x)) 2)                              4,;test single arg function
  "{{fun {x} {* x x}} 2}"                             4,;test single arg function
  '((fun (x y) (* x y)) 2 3)                          6,;test 2 arg function
  "{{fun {x y} {* x y}} 2 3}"                         6,;test 2 arg function
  '((fun (x y z) (* x (+ y z))) 2 1 2)                6,;test 3 arg function
  "{{fun {x y z} {* x {+ y z}}} 2 1 2}"               6,;test 3 arg function
  '((fun (x) (with ((y 3)) (* y x))) 2)               6,;test with inside function
  "{{fun {x} {with {{y 3}} {* y x}}} 2}"              6 ;test with inside function
}); simple-tests

(def simple-failure-tests (list 
  '(),                                       ;test nothing!
  '(+ 3),                                    ;addition test for insufficient args
  '(- 3),                                    ;subtraction test for insufficient args
  '(* 3),                                    ;multiplication test for insufficient args
  '(/ 3),                                    ;division test for insufficient args
  '(+ x 4),                                  ;addition test for unbound identifiers
  '(- x 4),                                  ;subtraction test for unbound identifiers
  '(* x 4),                                  ;multiplication test for unbound identifiers
  '(/ x 4),                                  ;division test for unbound identifiers
  '(with ((x (+ 5 5)) (x (+ 3 3))) (+ x x)), ;test for duplicate identifiers
  '(with ((1 (+ 5 5)) (x (+ 3 3))) (+ x x)), ;test for numeric identifiers
  '((fun (x x) (* x x)) 2 3),                ;test for duplicate identifiers
  '((fun (x y) (* x y)) 2),                  ;test for insufficient arguments
  '((fun (x 1) (* x 1)) 2 3),                ;test for numeric identifiers
  '(with ((x 10) (x 20)) 50)                 ;test given as example
)); simple-failure-tests

(def interp-failure-tests (list 
  '(unknown 2) ;test unknown operator
)); interp-failure-tests

(defn run-test
  "Test programs"
  [exec program expected testid]
  (try 
	(let [result (exec program)]
      (if (not= result expected) 
        (str "Test " (str testid) " Failed: " program " = " result)
        (str "Test " (str testid) " Succeeded: " program " = " result)))
    (catch clojure.lang.ExceptionInfo e
      (if (= :parser (-> e ex-data :cause))
        (str "Test " (str testid) " Failed with parser error: " (.getMessage e) "(" program ")")
        (str "Test " (str testid) " Failed with unknown error: " (.getMessage e) "(" program ")"))
    )
    (catch Exception e
        (str "Test " (str testid) " Failed miserably: " (.getMessage e) ", (" program ")")
    )
  )
); run-test
(defn run-failure-test
  "Run tests that are intended to produce errors"
  [exec program testid]
  (try 
    (let [result (exec program)]
      (str "Test " (str testid) " Failed: " (str program) " = " result))
    (catch clojure.lang.ExceptionInfo e
      (if (= :parser (-> e ex-data :cause))
        (str "Test " (str testid) " Succeeded at generating a correct error: " (.getMessage e))
        (str "Test " (str testid) " Failed at generating a correct error: " (.getMessage e)))
    )
    (catch Exception e
        (str "Test " (str testid) " Failed miserably: " (.getMessage e) ", (" program ")")
    )
  )
); run-failure-test
(defn run-tests
  "Runs all tests"
  []
  (clojure.string/join "\n" (flatten (list
    (doall (map (fn [a b c] (run-test xinterp.core/run a b c))                (keys simple-tests) (vals simple-tests) (drop 1 (range))))
    (doall (map (fn [a b]   (run-failure-test xinterp.core/run a b))          simple-failure-tests (drop (+ 1 (count simple-tests)) (range))))
    (doall (map (fn [a b]   (run-failure-test xinterp.core/interp a b))       interp-failure-tests (drop (+ 1 (count simple-tests) (count simple-failure-tests)) (range))))
    (doall (map (fn [a b c] (run-test xinterp_lazy.core/run a b c))           (keys simple-tests) (vals simple-tests) (drop (+ 1 (count simple-tests) (count simple-failure-tests) (count interp-failure-tests)) (range))))
    (doall (map (fn [a b]   (run-failure-test xinterp_lazy.core/run a b))     simple-failure-tests (drop (+ 1 (* (count simple-tests) 2) (count simple-failure-tests) (count interp-failure-tests)) (range))))
    (doall (map (fn [a b]   (run-failure-test xinterp_lazy.core/interp a b))  interp-failure-tests (drop (+ 1 (* (count simple-tests) 2) (* (count simple-failure-tests) 2) (count interp-failure-tests)) (range))))
  )))
); run-tests
(println (run-tests))
;(run-tests)