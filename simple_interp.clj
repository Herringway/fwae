(ns parse-interp.core
  (:gen-class))

;Initial parser created by Federico Mora, 2014
;Modified by Cameron Ross, 2014

(def pbinfunctable {"+" "add", "-" "sub", "*" "mul", "/" "div"})
(def ibinfunctable {"add" +  , "sub" -  , "mul" *  , "div" /  })
(declare interp)
(defn parse
	"Parser for WAE language, output is intended to be piped into FWAE interpreter.

	Will recurse using the following grammar:
	<FWAE> ::= <num> 
		| {+ <FWAE> <FWAE>} 
		| {- <FWAE> <FWAE>} 
		| {* <FWAE> <FWAE>} 
		| {/ <FWAE> <FWAE>} 
		| <id> 
		| {with {{<id> <FWAE>} ...} <FWAE>} 
		| {fun {<id> ...} <FWAE>} 
		| {<FWAE> <FWAE> ...}

	Base case is just a number or an identifier. Will recurse if first symbol is an operator (+, -, *, /, with)
	"
	[fwae]
	;check type
	(cond
		(not= (type fwae) clojure.lang.PersistentList ) ;check if argument passed in is a list
			fwae

		:else ;looks like we got a list here
			(cond 
				(contains? pbinfunctable (str (first fwae)))	; Is the first argument in the binary function table?
					(if (< (count fwae) 3) (throw (Exception. "Parser error: Insufficient number of arguments for binary function"))
					(list (get pbinfunctable (str (first fwae))) (parse (second fwae)) (parse (nth fwae 2)))) ;Use the parser binary function lookup table for most ops
				(= "with" (str (first fwae)))                ; Is this a with statement?
					(if (not= (distinct (map first (second fwae))) (map first (second fwae))) (throw (Exception. "Parser error: Duplicate identifiers"))
					(list "with" (parse (second fwae)) (parse (nth fwae 2)))) ;In the parser, with merely needs some extra error checking
				(= "fun" (str (first fwae)))                ; Is this a function statement?
					(if (not= (distinct (second fwae)) (second fwae)) (throw (Exception. "Parser error: Duplicate identifiers"))
					(list "fun" (parse (second fwae)) (parse (nth fwae 2)))) ;In the parser, with merely needs some extra error checking
				:else (map parse fwae) ; Parsing identifiers. likely in the first arg of a with statement
			) ;cond
	) ;check type
);parse
(defn interp-o
	"Interpreter for the FWAE language. Will hopefully output a number corresponding to the parsed input.
	Intended to be used with the FWAE parser.
	"
	([fwae idtable]
		(cond
			(= (type       (first fwae))  clojure.lang.PersistentList)
				(interp-o (first fwae) idtable) ; if it's a list, just try interpretting it
			(= "num"  (str (first fwae))) 
				(if (number? (second fwae)) (second fwae)
				(throw (Exception. "Interpreter error: num with a non-number!"))) ; just a number. return it.
			(contains? ibinfunctable (str (first fwae)))
				((get ibinfunctable (str (first fwae))) (interp-o (second fwae) idtable) (interp-o (nth fwae 2) idtable)) ; use the interpreter binary function lookup table if possible
			(= "with" (str (first fwae))) 
				(if (not= (distinct (map first (map first (second fwae)))) (list "id")) (throw (Exception. "Interpreter error: cannot bind non-identifiers"))
				(let [varids (map second (map first (second fwae)))]                                    ; the second part of the first part of the first argument should always be an identifier, so let's grab those
				(let [varvals (map (fn [x] (interp-o x idtable)) (map second (second fwae)))]             ; following that will be a FWAE statement to interpret, interpret and grab the results
					(interp-o (nth fwae 2) (into {} (map (fn [a b] (assoc idtable a b)) varids varvals))) ; combine the two and interpret the second with argument using the new idtable
				) ;varvals
				) ;varids
				) ;if                                                                                  ; handle with statements
			(= "fun" (str (first fwae)))
				(do 
					(if (not= (distinct (map first (second fwae))) (list "id")) (throw (Exception. "Interpreter error: cannot bind non-identifiers")))
					(println (list (vec (map second (second fwae))) (nth fwae 2)))
				)
			(= "id"   (str (first fwae)))
				(if (contains? idtable (second fwae))
				(get idtable (second fwae))
				(throw (Exception. "Interpreter error: Identifier undefined"))); identifiers are all in the idtable
			:else (throw (Exception. "Interpreter error: Unknown operator reached"))                       ; what is this mess?! don't give me input I don't understand!
		) ;cond
	) ;with idtable
	([fwae]
		(interp-o fwae {})
	)
);interp
(defn build-func
	[params expr idtable]
		(do
		(println (map (fn [z] (list (first z) 1)) params))
		(fn [x y] (interp expr (into {} (map (fn [a] (assoc idtable (first a) (second a))) (map (fn [z] (list (first z) x)) params))))))
		;(println params)
		;(throw (Exception. "Fun unimplemented"))
)
(defn interp
	"Interpreter for the FWAE language. Will hopefully output a number corresponding to the parsed input.
	Intended to be used with the FWAE parser.
	"
	([fwae idtable]
		(do
			;(println (str "interpreting " fwae ", " (type fwae)))
			;(println idtable)
		(cond
			(number? fwae)
				fwae
			(symbol? fwae)
				(if (contains? idtable fwae)
				(get idtable fwae)
				(throw (Exception. (str "Interpreter error: Identifier undefined: " fwae))))
			(coll? fwae)
				(cond 
					(= "fun" (str (first fwae)))
						(build-func (second fwae) (nth fwae 2) idtable)
					(= "with" (str (first fwae)))
						(interp (nth fwae 2) (into {} (map (fn [a] (assoc idtable (first a) (second a))) (map (fn [x] (list (first x) (interp (second x) idtable))) (second fwae)))))
					(= (count fwae) 3)
						(let [func (interp (first fwae) idtable)]
							(if (fn? func)
								(func (interp (second fwae) idtable) (interp (nth fwae 2) idtable))
								(throw (Exception. (str "Not a binary function: " func))))
						)
					(= (count fwae) 2)
						(let [func (interp (first fwae) idtable)]
							(if (fn? func)
								(func (interp (second fwae) idtable))
								(throw (Exception. (str "Not a unary function: " func))))
						)
					(= (count fwae) 1)
						(interp (first fwae) idtable)
					:else (throw (Exception. "Invalid list size")))
			(contains? ibinfunctable (str fwae))
				(get ibinfunctable (str fwae))
			(= "id" (str fwae))
				identity
			:else (throw (Exception. (str "Unknown input: " fwae)))
		)))
	([fwae]
		(interp fwae {}))
);interp

(defn run
  	"calls the parse and interp with given fwae"
	[fwae]
	(do
	(println "executing")
	(interp (parse fwae)))
 ) ;run
 
 (defn run-str
	[program]
	(run (load-string (str "'" (clojure.string/replace program #"\{|\}" {"{" "(" "}" ")"}))))
 ) ;run-str
