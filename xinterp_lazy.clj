(ns xinterp_lazy.core
  (:gen-class))

;Initial parser created by Federico Mora, 2014
;Modified by Cameron Ross, 2014

(def pbinfunctable {"+" "add", "-" "sub", "*" "mul", "/" "div"})
(def ibinfunctable {"add" +  , "sub" -  , "mul" *  , "div" /  })
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
  "Lazy Interpreter for the FWAE language. Will hopefully output a number corresponding to the parsed input.
  Intended to be used with the FWAE parser.
  "
  ([fwae idtable]
    (cond
      (number? fwae)           ;if it's a number,
        fwae                   ;just return it
      (symbol? fwae)                   ;symbols represent identifiers
        (if (contains? idtable fwae)   ;check if the identifier is bound
          (get idtable fwae)           ;return value if so
          (throw (ex-info (str "Interpreter error: Identifier undefined: " fwae) {:cause :parser})))
      (coll? fwae)    ;are we dealing with a list of FWAEs?
        (cond 
          (= "fun" (str (first fwae))) ;build a function!
            (if (= (some number? (second fwae)) true) ;check if numbers snuck into the argument list
			  (throw (ex-info (str "Interpreter error: Invalid identifier (numeric)") {:cause :parser})) ;numeric identifiers are illegal
              (fn [& args] (interp (nth fwae 2) (into {} (map (fn [a b] (assoc idtable a b)) (second fwae) args))))) ;return a function that interprets the associated expression with identifiers properly bound
          (= "with" (str (first fwae))) ;dealing with with statements
            (if (= (some number? (map first (second fwae))) true) ;check if numbers snuck into the identifier list
			  (throw (ex-info (str "Interpreter error: Invalid identifier (numeric)") {:cause :parser})) ;numeric identifiers are illegal
              (interp (nth fwae 2) (into {} (map (fn [a] (assoc idtable (first a) (second a))) (map (fn [x] (list (first x) (interp (second x) idtable))) (second fwae)))))) ;bind the identifiers and interpret the expression
          (> (count fwae) 0)
            (let [func (interp (first fwae) idtable)]                                ;evaluate the first element as a function
              (if (fn? func)                                                              ;make sure we've actually got a function
                (apply func (map (fn [x] (interp x idtable)) (rest fwae)))           ;call the function with the remaining elements, interpreted
                (if (= (count fwae) 1)                                                    ;otherwise check if it's a single element list
                  (interp (first fwae) idtable)                                      ;a single element list is okay
                  (throw (ex-info (str "Not a function: " func) {:cause :parser})))) ;otherwise it's an error
            )
          :else (throw (ex-info "Invalid list" {:cause :parser}))) ;empty list??? that's an error
      (contains? ibinfunctable (str fwae)) ;is this a built-in function, perhaps?
        (get ibinfunctable (str fwae)) ;yep, let's use that
      :else (throw (ex-info (str "Unknown input: " fwae) {:cause :parser})) ;can't deal with unknown input, throw error
    ))
  ([fwae]
    (interp fwae {})) ;interpret our fwae with an empty idtable
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