(ns xinterp_lazy.core
  (:gen-class))

;Initial parser created by Federico Mora, 2014
;Modified by Cameron Ross, 2014

(def pbinfunctable-lazy {"+" "add", "-" "sub", "*" "mul", "/" "div"})
(def ibinfunctable-lazy {"add" +  , "sub" -  , "mul" *  , "div" /  })
(defn parse-lazy
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
        (contains? pbinfunctable-lazy (str (first fwae)))  ; Is the first argument in the binary function table?
          (if (< (count fwae) 3) (throw (ex-info "Parser error: Insufficient number of arguments for binary function" {:cause :parser}))
          (list (get pbinfunctable-lazy (str (first fwae))) (parse-lazy (second fwae)) (parse-lazy (nth fwae 2)))) ;Use the parser binary function lookup table for most ops
        (= "with" (str (first fwae)))                ; Is this a with statement?
          (if (not= (distinct (map first (second fwae))) (map first (second fwae))) (throw (ex-info "Parser error: Duplicate identifiers" {:cause :parser}))
          (list "with" (parse-lazy (second fwae)) (parse-lazy (nth fwae 2)))) ;In the parser, with merely needs some extra error checking
        (= "fun" (str (first fwae)))                ; Is this a function statement?
          (if (not= (distinct (second fwae)) (second fwae)) (throw (ex-info "Parser error: Duplicate identifiers" {:cause :parser}))
          (list "fun" (parse-lazy (second fwae)) (parse-lazy (nth fwae 2)))) ;In the parser, with merely needs some extra error checking
        :else (map parse-lazy fwae) ; Parsing identifiers. likely in the first arg of a with statement
      ) ;cond
  ) ;check type
);parse-lazy
(defn interp-lazy
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
            (cond
              (= (some number? (second fwae)) true) (throw (ex-info (str "Interpreter error: Invalid identifier (numeric)") {:cause :parser})) ;make sure no numbers snuck into the argument list
              :else (fn [& args] (interp-lazy (nth fwae 2) (into {} (map (fn [a b] (assoc idtable a b)) (second fwae) args)))))
          (= "with" (str (first fwae)))
            (if (= (some number? (map first (second fwae))) true) (throw (ex-info (str "Interpreter error: Invalid identifier (numeric)") {:cause :parser}))
            (interp-lazy (nth fwae 2) (into {} (map (fn [a] (assoc idtable (first a) (second a))) (map (fn [x] (list (first x) (interp-lazy (second x) idtable))) (second fwae))))))
          (> (count fwae) 0)
            (let [func (interp-lazy (first fwae) idtable)]                                ;evaluate the first element as a function
              (if (fn? func)                                                         ;make sure we've actually got a function
                (apply func (map (fn [x] (interp-lazy x idtable)) (rest fwae)))           ;call the function with the remaining elements, interpreted
                (if (= (count fwae) 1)                                               ;otherwise check if it's a single element list
                  (interp-lazy (first fwae) idtable)                                      ;a single element list is okay
                  (throw (ex-info (str "Not a function: " func) {:cause :parser})))) ;otherwise it's an error
            )
          :else (throw (ex-info "Invalid list" {:cause :parser})))
      (contains? ibinfunctable-lazy (str fwae))
        (get ibinfunctable-lazy (str fwae))
      (= "id" (str fwae))
        identity
      :else (throw (ex-info (str "Unknown input: " fwae) {:cause :parser}))
    ))
  ([fwae]
    (interp-lazy fwae {}))
);interp-lazy

(defn run-lazy
    "calls the parse and interp with given fwae. if a string, turn it into a sequence"
  [fwae]
  (interp-lazy 
    (parse-lazy 
      (if (string? fwae) 
        (load-string (str "'" (clojure.string/replace fwae #"\{|\}" {"{" "(" "}" ")"}))) 
        fwae
      )
    )
  )
) ;run-lazy