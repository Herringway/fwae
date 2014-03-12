(ns parse-interp.core
  (:gen-class))

;Initial parser created by Federico Mora, 2014
;Modified by Cameron Ross, 2014

(defn pars
	"parser for ae language, output is intended to be piped into interpreter.

	Will recurse using grammar:
	<AE> ::= <num> 
	 | {+ <AE> <AE>} 
	 | {- <AE> <AE>}

	Base case is just a number. Will recurse if first symbol is an operator (+/-)
	"
	[ae]
	;check type
	(if-not (= (type ae) clojure.lang.PersistentList ) ;check if argument passed in is a list
		(if (number? ae) ;check if its a number
			(list "num" ae)

			(println "Error,", ae, "is not a valid number or symbol") ;will happen is not a list or a number
		) ;end of if its a number

		;else if it is a list this next part will exectute 
		(let [leftmost (nth ae 0)]
		(let [loperand (nth ae 1 "Didn't find loperand")] ;might not be there
		(let [roperand (nth ae 2 "Didn't find roperand")] ;might not be there

			(if (= "+" (str leftmost)) ;is leftmost a plus sign?
				;return a length 3 list. The second two entries are recursive 
				;calls on aprs with the left and right operands
				(list "add" (pars loperand) (pars roperand)
				) ;from print inside plus check

				(if (= "-" (str leftmost)) ;else is it a minus sign?
					;return a length 3 list. The second two entries are recursive 
					;calls on aprs with the left and right operands
					(list "sub" (pars loperand) (pars roperand) )

					(println "Error,", leftmost, "is not a valid operator") ;reaches this line if it is not a subtraction or addition

				) ;sub (if that is nested)

			) ;plus (bigger if)

		) ;let roperand
		) ;let loperand
		) ;let leftmost
		;end of else (from if this is a list)

	) ;check type

);pars

(defn -main
  	"calls the pars and interp with given ae"
	[ae]
	(println (pars ae)) ;will soon pipe this into interp (which doesn't exist yet)
 ) ;-main
