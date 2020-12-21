import Data.Maybe
import  Data.List

starting = [9,19,1,6,0,5,4]

solve :: [Int]
solve = starting ++ [y| 
    x<-[(length starting - 1)..], 
    last <- [solve !! x], 
    found <- [elemIndex last ((reverse.take x) solve)],
    z <- [fromMaybe (-1) found],
    y <- [z + 1]
    ]