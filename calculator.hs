import Data.List
import System.Environment

main = do
    expression <- getArgs
    if null expression
      then print "No input given!"
      else print ( solveRPN ( head expression ))



solveRPN :: String -> Double
solveRPN = head . foldl foldingFunc [] . words
    where foldingFunc (x:y:ys) "*" = (y * x):ys
          foldingFunc (x:y:ys) "+" = (y + x):ys
          foldingFunc (x:y:ys) "-" = (y - x):ys
          foldingFunc (x:y:ys) "/" = (y / x):ys
          foldingFunc (x:y:ys) "^" = (y ** x):ys
          foldingFunc (x:xs) "ln" = log x:xs
          foldingFunc xs "sum" = [sum xs]
          foldingFunc xs numStr = read numStr:xs
