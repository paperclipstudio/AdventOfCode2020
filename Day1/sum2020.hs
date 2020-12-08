
import System.IO

sum2020 [] = 0
sum2020 (x:xs)
    | (2020 - x)  `elem` xs = x * (2020-x)
    | otherwise =  sum2020 xs

main = do
    handle <- readFile "data.txt"
    let line = lines handle
        nums = map read line :: [Int]
    putStr $ show $ sum2020 nums

    