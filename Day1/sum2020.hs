
import System.IO

sum2020 [] = 0
sum2020 (x:xs)
    | (2020 - x)  `elem` xs = x * (2020-x)
    | otherwise =  sum2020 xs


main = apply $  threeSum2020

apply func = do
    handle <- readFile "data.txt"
    let line = lines handle
        nums = map read line :: [Int]
    putStr $ show $ func nums

-- Part 2
sumGen _ [] = 0
sumGen target (x:xs)
    | (target - x) `elem` xs = x * (target - x)
    | otherwise = sumGen target xs   

threeSum2020 (x:xs) =
    let result =  x * sumGen (2020 - x) xs in
    if result /= 0 then result else threeSum2020 xs