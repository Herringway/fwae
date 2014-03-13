(ns parse-interp.core
  (:gen-class))

;Initial parser created by Federico Mora, 2014
;Modified by Cameron Ross, 2014

(declare pwith)
(declare iwith)

(def pbinfunctable {"+" "add", "-" "sub", "*" "mul", "/" "div"})
(def ibinfunctable {"add" +  , "sub" -  , "mul" *  , "div" /  })

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
			(cond 
				(contains? pbinfunctable (str (first wae)))	; Is the first argument in the binary function table?
					(if (< (count wae) 3) (throw (Exception. "Parser error: Insufficient number of arguments for binary function"))
					(list (get pbinfunctable (str (first wae))) (pars (second wae)) (pars (nth wae 2)))) ;Use the parser binary function lookup table for most ops
				(= "with" (str (first wae)))                ; Is this a with statement?
					(if (not= (distinct (map first (second wae))) (map first (second wae))) (throw (Exception. "Parser error: Duplicate identifiers"))
					(list "with" (pars (second wae)) (pars (nth wae 2)))) ;In the parser, with merely needs some extra error checking
				
				:else (map pars wae) ; Parsing identifiers. likely in the first arg of a with statement
			) ;cond
	) ;check type
);pars
(defn interp
	"Interpreter for the WAE language. Will hopefully output a number corresponding to the parsed input.
	Intended to be used with the WAE parser.
	"
	([wae idtable]
		(cond
			(= (type       (first wae))  clojure.lang.PersistentList)
				(interp (first wae) idtable) ; if it's a list, just try interpretting it
			(= "num"  (str (first wae))) 
				(if (number? (second wae)) (second wae)
				(throw (Exception. "Interpreter error: num with a non-number!"))) ; just a number. return it.
			(contains? ibinfunctable (str (first wae)))
				((get ibinfunctable (str (first wae))) (interp (second wae) idtable) (interp (nth wae 2) idtable)) ; use the interpreter binary function lookup table if possible
			(= "with" (str (first wae))) 
				(if (not= (distinct (map first (map first (second wae)))) (list "id")) (throw (Exception. "Interpreter error: cannot bind non-identifiers"))
				(let [varids (map second (map first (second wae)))]                                    ; the second part of the first part of the first argument should always be an identifier, so let's grab those
				(let [varvals (map (fn [x] (interp x idtable)) (map second (second wae)))]             ; following that will be a WAE statement to interpret, interpret and grab the results
					(interp (nth wae 2) (into {} (map (fn [a b] (assoc idtable a b)) varids varvals))) ; combine the two and interpret the second with argument using the new idtable
				) ;varvals
				) ;varids
				) ;if                                                                                  ; handle with statements
			(= "id"   (str (first wae)))
				(if (contains? idtable (second wae))
				(get idtable (second wae))
				(throw (Exception. "Interpreter error: Identifier undefined"))); identifiers are all in the idtable
			:else (throw (Exception. "Interpreter error: Unknown operator reached"))                       ; what is this mess?! don't give me input I don't understand!
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
 
 (defn run-str
	[str]
	(run (clojure.string/replace str #"\{|\}" {"{" "(" "}" ")"}))
 ) ;run-str
