type Operator = Double -> Double -> Double
type OperatorStructure = (String, Operator)
type Operators = [OperatorStructure]
registredOperators :: Operators
registredOperators = [
                ("+", (+)),
                ("-", (-)),
                ("/", (/)),
                ("*", (*))
            ]
main = print $ calculateEvaluation "12 * 3 - 4 + 6"

calculateEvaluation :: String -> Double
calculateEvaluation = calcIteration registredOperators . words
            
calcIteration :: Operators -> [String] -> Double
calcIteration _ [number] = read number
calcIteration ((operator, function):xs) unparsed =
    case span (/=operator) unparsed of
        (_, []) -> calcIteration xs unparsed
        (beforeOperator, afterOperator) -> 
            function
                (calcIteration registredOperators beforeOperator)
                (calcIteration registredOperators $ drop 1 afterOperator)