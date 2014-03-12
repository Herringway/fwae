(ns parse-interp.core
  (:gen-class))

;Initial parser created by Federico Mora, 2014
;Modified by Cameron Ross, 2014

(defn pars
	"parser for ae language, output is intended to be piped into interpreter.

	Will recurse using grammar:
	<WAE> ::= <num> 
	 | {+ <WAE> <WAE>} 
	 | {- <WAE> <WAE>}
	 | {* <WAE> <WAE>}
	 | {/ <WAE> <WAE>}
	 | {with {{<id> <WAE>} ...} <WAE>}
	 | <id>

	Base case is just a number. Will recurse if first symbol is an operator (+, -, *, /, with)
	"
	[ae]
	;check type
	(if-not (= (type ae) clojure.lang.PersistentList ) ;check if argument passed in is a list
		(cond 
			(number? ae) (list "num" ae) ; if number, prepend "num"
			:else (list "id" ae) ;otherwise it's an identifier
		) ;end of if its a number or identifier

		;else if it is a list this next part will exectute 
		(let [leftmost (nth ae 0)]
		(let [loperand (nth ae 1 "Didn't find loperand")] ;might not be there
		(let [roperand (nth ae 2 "Didn't find roperand")] ;might not be there
		(cond 
			(= "+" (str leftmost)) (list "add" (pars loperand) (pars roperand)) ;if +, return a length 3 list with "add" and the operands
			(= "-" (str leftmost)) (list "sub" (pars loperand) (pars roperand)) ;if -, return a length 3 list with "sub" and the operands
			(= "*" (str leftmost)) (list "mul" (pars loperand) (pars roperand)) ;if *, return a length 3 list with "mul" and the operands
			(= "/" (str leftmost)) (list "div" (pars loperand) (pars roperand)) ;if /, return a length 3 list with "div" and the operands
			(= "with" (str leftmost)) (list "with" (pars loperand) (pars roperand)) ;with works pretty much the same way in the parser
			:else (map pars ae) ;parsing the first arg for with here
		) ;cond
		) ;let roperand
		) ;let loperand
		) ;let leftmost
	) ;check type

);pars


(defn interp
	[ae]
	(println ae)
);interp

(defn -main
  	"calls the pars and interp with given ae"
	[ae]
	(interp (pars ae)) ;will soon pipe this into interp (which doesn't exist yet)
 ) ;-main
