(ns xinterp.core
  (:gen-class))

;Initial parser created by Federico Mora, 2014
;Modified by Cameron Ross, 2014

(def pbinfunctable {"+" "add", "-" "sub", "*" "mul", "/" "div"})
(def ibinfunctable {"add" +  , "sub" -  , "mul" *  , "div" /  })

(defn popn
  "Pops n items off of a sequence. Exact behaviour depends on type of sequence"
  [sequence count]
  (if (= count 0) ;anything left to pop?
      sequence ; return sequence
      (popn (pop sequence) (- count 1)) ;else pop another item off the sequence
  )
);popn

(defn parse
  "Parser for FWAE language, output is intended to be piped into FWAE interpreter.

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
    (coll? fwae) ;check if argument passed in is a list
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

    :else ;looks like we got a list here
      fwae
  ) ;check type
);parse
(defn interp
  "Eager interpreter for the FWAE language. Will hopefully output a number corresponding to the parsed input.
  Intended to be used with the FWAE parser.
  "
  ([fwae idtable stack]
    (cond
      (number? fwae)           ;if it's a number,
        fwae                   ;just return it
      (symbol? fwae)                   ;symbols represent identifiers
        (if (contains? idtable fwae)   ;check if the identifier is bound
          (get idtable fwae)           ;return value if so
          (throw (ex-info (str "Interpreter error: Identifier undefined: " fwae) {:cause :parser}))) ;identifier unbound. someone made an error
      (coll? fwae)    ;are we dealing with a list of FWAEs?
        (cond 
		  (= (count fwae) 0) ;empty list?
             (throw (ex-info "Invalid list" {:cause :parser})) ;kill this error with fire
		  (= (count fwae) 1) ;determine if we have anything to push onto the stack
            (interp (first fwae) idtable stack) ;interpret the fwae and push nothing onto the stack (nothing to push)
          :else
            (interp (first fwae) idtable (apply conj stack (rest fwae)))) ;interpret the fwae and push the rest of the items onto the stack
      (contains? ibinfunctable (str fwae)) ;are we interpreting one of the built-in functions?
        ((get ibinfunctable (str fwae)) (interp (second stack) idtable (popn stack 2)) (interp (first stack) idtable (popn stack 2))) ;interpret it with the top two items on the stack, which are also interpreted
      (= "fun" (str fwae)) ;looks like a function
        (if (> (+ (count (second stack)) 2) (count stack)) ;determine if we have enough items on the stack to interpret this function
          (throw (ex-info (str "Interpreter error: Insufficient arguments for function") {:cause :parser})) ;not enough items on the stack!
          (let [varids (second stack)]    ; the second part of the first part of the first argument should always be an identifier, so let's grab those
            (if (= (some number? varids) true) (throw (ex-info (str "Interpreter error: Invalid identifier (numeric)") {:cause :parser})) ;numeric identifiers are illegal
            (let [varvals (popn stack 2)]             ; the next items on the stack should be values we can use
              (interp (first stack) (into {} (map (fn [a b] (assoc idtable a b)) varids varvals)) (popn stack (+ 2 (count varids)))) ; associate the identifiers with their values and interpret this function with them
            )) ;varvals
          )) ;varids
      (= "with" (str fwae)) ;looks like a with statement
        (let [varids (map first (second stack))] ; the first part of the second item on the stack will be an identifier
          (if (= (some number? varids) true) (throw (ex-info (str "Interpreter error: Invalid identifier (numeric)") {:cause :parser})) ;numeric identifiers are illegal
          (let [varvals (map (fn [x] (interp x idtable stack)) (map second (second stack)))] ;the second part of the second item on the stack will be values to bind. interpret and prepare them for binding
            (interp (first stack) (into {} (map (fn [a b] (assoc idtable a b)) varids varvals)) (popn stack 2)) ;interpret the first item on the stack (our expression) with the identifiers and their bound values
          ))
        )
      :else (throw (ex-info (str "Unknown input: " fwae) {:cause :parser})) ;can't parse unknown input, so throw an error
    ))
  ([fwae]
    (interp fwae {} ())) ;interpret the fwae with an empty id table and stack
);interp


(defn run
  "Executes an FWAE statement by running it through an FWAE parser and interpreter."
  [fwae]
  (interp
    (parse
      (if (string? fwae) ;check if our program is a string
        (load-string (str "'" (clojure.string/replace fwae #"\{|\}" {"{" "(" "}" ")"})))  ;turn the string into a list
        fwae ;else just use it as-is
      )
    )
  )
) ;run