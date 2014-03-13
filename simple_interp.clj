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
				(= "with" (str leftmost)) (do                                           ;in the parser, with isn't too special, but we need some extra error checking
											(if (not= (distinct (map first loperand)) (map first loperand)) (throw (Exception. "Interpreter error: Duplicate identifiers")))
											(list "with" (pars loperand) (pars roperand)))
				
				:else (map pars wae) ;parsing the first arg for with here
			) ;cond
			) ;let roperand
			) ;let loperand
			) ;let leftmost
	) ;check type
);pars

(defn interp
	"Interpreter for the WAE language. Will hopefully output a number corresponding to the parsed input.
	Intended to be used with the WAE parser.
	"
	([wae idtable]
		(cond
			(= (type       (first wae))  clojure.lang.PersistentList) (interp (first wae) idtable)
			(= "num"  (str (first wae))) (second wae)                                    ; just a number.
			(= "add"  (str (first wae))) (+ (interp (second wae) idtable) (interp (nth wae 2) idtable))  ; adding two things together
			(= "sub"  (str (first wae))) (- (interp (second wae) idtable) (interp (nth wae 2) idtable))  ; handling subtraction
			(= "mul"  (str (first wae))) (* (interp (second wae) idtable) (interp (nth wae 2) idtable))  ; multiplication!
			(= "div"  (str (first wae))) (/ (interp (second wae) idtable) (interp (nth wae 2) idtable))  ; division probably
			(= "with" (str (first wae))) (do
											(if (not= (distinct (map first (map first (second wae)))) (list "id")) (throw (Exception. "Parser error: cannot bind non-identifiers")))
											(let [varids (map second (map first (second wae)))]                                    ; the second part of the first part of the first argument should always be an identifier
											(let [varvals (map (fn [x] (interp x idtable)) (map second (second wae)))]             ; following that will be a WAE statement to interpret
												(interp (nth wae 2) (into {} (map (fn [a b] (assoc idtable a b)) varids varvals))) ; combine the two and interpret the second argument using the new idtable
											)
											)
											)			; with statements!
			(= "id"   (str (first wae))) (do 
											(if (contains? idtable (second wae))
											(get idtable (second wae))
											(throw (Exception. "Parser error: Identifier undefined")))); identifiers are all in the idtable
			:else (throw (Exception. "Unknown operator reached!"))                       ; what is this mess?! don't give me input I don't understand!
		) ;cond
	) ;with idtable
	([wae]
		(interp wae {})
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
