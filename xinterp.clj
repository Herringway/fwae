(ns xinterp.core
  (:gen-class))

;Initial parser created by Federico Mora, 2014
;Modified by Cameron Ross, 2014

(def pbinfunctable {"+" "add", "-" "sub", "*" "mul", "/" "div"})
(def ibinfunctable {"add" +  , "sub" -  , "mul" *  , "div" /  })

(defn popn [x y]
  (if (= y 0) 
  x 
  (popn (pop x) (- y 1)))
);popn

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
        (contains? pbinfunctable (str (first fwae)))  ; Is the first argument in the binary function table?
          (if (< (count fwae) 3) (throw (ex-info "Parser error: Insufficient number of arguments for binary function" {:cause :parser}))
          (list (get pbinfunctable (str (first fwae))) (parse (second fwae)) (parse (nth fwae 2)))) ;Use the parser binary function lookup table for most ops
        (= "with" (str (first fwae)))                ; Is this a with statement?
          (if (not= (distinct (map first (second fwae))) (map first (second fwae))) (throw (ex-info "Parser error: Duplicate identifiers" {:cause :parser}))
          (list "with" (parse (second fwae)) (parse (nth fwae 2)))) ;In the parser, with merely needs some extra error checking
        (= "fun" (str (first fwae)))                ; Is this a function statement?
          (if (not= (distinct (second fwae)) (second fwae)) (throw (ex-info "Parser error: Duplicate identifiers" {:cause :parser}))
          (list "fun" (parse (second fwae)) (parse (nth fwae 2)))) ;In the parser, with merely needs some extra error checking
        :else (map parse fwae) ; Parsing identifiers. likely in the first arg of a with statement
      ) ;cond
  ) ;check type
);parse
(defn interp
  "Interpreter for the FWAE language. Will hopefully output a number corresponding to the parsed input.
  Intended to be used with the FWAE parser.
  "
  ([fwae idtable stack]
    (cond
      (number? fwae)           ;if it's a number,
        fwae                   ;just return it
      (symbol? fwae)                   ;symbols represent identifiers
        (if (contains? idtable fwae)   ;check if the identifier is bound
          (get idtable fwae)           ;return value if so
          (throw (ex-info (str "Interpreter error: Identifier undefined: " fwae) {:cause :parser})))
      (coll? fwae)    ;are we dealing with a list of FWAEs?
		(if (= (count fwae) 1)
		(interp (first fwae) idtable stack)
		(interp (first fwae) idtable (apply conj stack (rest fwae))))
      (contains? ibinfunctable (str fwae))
        ((get ibinfunctable (str fwae)) (interp (second stack) idtable (pop (pop stack))) (interp (first stack) idtable (popn stack 2)))
      (= "fun" (str fwae))
		(if (> (+ (count (second stack)) 2) (count stack))
          (throw (ex-info (str "Interpreter error: Insufficient arguments for function") {:cause :parser}))
		  (let [varids (second stack)]                                    ; the second part of the first part of the first argument should always be an identifier, so let's grab those
            (if (= (some number? varids) true) (throw (ex-info (str "Interpreter error: Invalid identifier (numeric)") {:cause :parser}))
			(let [varvals (popn stack 2)]             ; following that will be a WAE statement to interpret, interpret and grab the results
			  (interp (first stack) (into {} (map (fn [a b] (assoc idtable a b)) varids varvals)) (popn stack (+ 2 (count varids)))) ; combine the two and interpret the second with argument using the new idtable
			)) ;varvals
		  )) ;varids
      (= "with" (str fwae))
		(let [varids (map first (second stack))]
          (if (= (some number? varids) true) (throw (ex-info (str "Interpreter error: Invalid identifier (numeric)") {:cause :parser}))
		  (let [varvals (map (fn [x] (interp x idtable stack)) (map second (second stack)))]
            (interp (first stack) (into {} (map (fn [a b] (assoc idtable a b)) varids varvals)) (popn stack 2))
		  ))
		)
      :else (throw (ex-info (str "Unknown input: " fwae) {:cause :parser}))
    ))
  ([fwae]
    (interp fwae {} ()))
);interp


(defn run
    "calls the parse and interp with given fwae. if a string, turn it into a sequence"
  [fwae]
  (interp
    (parse
      (if (string? fwae) 
        (load-string (str "'" (clojure.string/replace fwae #"\{|\}" {"{" "(" "}" ")"}))) 
        fwae
      )
    )
  )
) ;run