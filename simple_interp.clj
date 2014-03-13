(ns parse-interp.core
  (:gen-class))

;Initial parser created by Federico Mora, 2014
;Modified by Cameron Ross, 2014

(declare pwith)
(declare iwith)
(defn pars
	"Parser for WAE language, output is intended to be piped into WAE interpreter.

	Will recurse using the following grammar:
	<WAE> ::= <num> 
	 | {+ <WAE> <WAE>} 
	 | {- <WAE> <WAE>}
	 | {* <WAE> <WAE>}
	 | {/ <WAE> <WAE>}
	 | {with {{<id> <WAE>} ...} <WAE>}
	 | <id>

	Base case is just a number or an identifier. Will recurse if first symbol is an operator (+, -, *, /, with)
	"
	[wae]
	;check type
	(cond
		(not= (type wae) clojure.lang.PersistentList ) ;check if argument passed in is a list
			(if (number? wae)
				(list "num" wae) ; if number, prepend "num"
				(list "id" wae)  ; otherwise it's an identifier
			) ;end of if its a number or identifier

		:else ;looks like we got a list here
			(let [leftmost (nth wae 0)]
			(let [loperand (nth wae 1 "Didn't find loperand")] ;might not be there
			(let [roperand (nth wae 2 "Didn't find roperand")] ;might not be there
			(cond 
				(= "+" (str leftmost))    (list "add"  (pars loperand) (pars roperand)) ;if +, return a length 3 list with "add" and the operands
				(= "-" (str leftmost))    (list "sub"  (pars loperand) (pars roperand)) ;if -, return a length 3 list with "sub" and the operands
				(= "*" (str leftmost))    (list "mul"  (pars loperand) (pars roperand)) ;if *, return a length 3 list with "mul" and the operands
				(= "/" (str leftmost))    (list "div"  (pars loperand) (pars roperand)) ;if /, return a length 3 list with "div" and the operands
				(= "with" (str leftmost)) (pwith loperand roperand) ;with works pretty much the same way in the parser, but we need some extra error checking
				
				:else (map pars wae) ;parsing the first arg for with here
			) ;cond
			) ;let roperand
			) ;let loperand
			) ;let leftmost
	) ;check type
);pars

(defn pwith
	"Handles with statements in the parser.
	Split into its own function in order to keep the parser tidy.
	Will throw an exception if duplicate identifiers are detected in the statement.
	"
	[ids wae]
	(do (if (not= (distinct (map first ids)) (map first ids)) (throw (Exception. "Duplicate identifiers"))))
	(list "with" (pars ids) (pars wae))
	
);with

(defn iwith
	"Handles with statements in the interpreter.
	Like with in the parser, this has been split into its own function to keep things tidy.
	"
	[ids evals]
	(println ids evals)
)

(defn interp
	"Interpreter for the WAE language. Will hopefully output a number corresponding to the parsed input.
	Intended to be used with the WAE parser.
	"
	([wae idtable]
		(cond
			(= (type       (first wae))  clojure.lang.PersistentList) (interp (first wae))
			(= "num"  (str (first wae))) (second wae)                                   ; just a number.
			(= "add"  (str (first wae))) (+ (interp (second wae)) (interp (nth wae 2))) ; adding two things together
			(= "sub"  (str (first wae))) (- (interp (second wae)) (interp (nth wae 2))) ; handling subtraction
			(= "mul"  (str (first wae))) (* (interp (second wae)) (interp (nth wae 2))) ; multiplication!
			(= "div"  (str (first wae))) (/ (interp (second wae)) (interp (nth wae 2))) ; division probably
			(= "with" (str (first wae))) (iwith (second wae) (nth wae 2))               ; with statements!
			:else (throw (Exception. "Unknown operator reached!"))                      ; what is this mess?! don't give me input I don't understand!
		) ;cond
	) ;with idtable
	([wae]
		(interp wae [])
	)
);interp

(defn run
  	"calls the pars and interp with given wae"
	[wae]
	(interp (pars wae))
 ) ;run
 
 (defn parsestr
	[str]
	(run (clojure.string/replace str #"\{|\}" {"{" "(" "}" ")"}))
 ) ;parsestr
