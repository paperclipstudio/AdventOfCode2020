import Data.List.Split ( splitOn )
import Data.List

main = do
    handle <- readFile "data"
    let group = splitOn [""] $ splitOn "\n" handle
    print $ sum $ map length $ map removeDup $ sortString $ joinGroups $ group

joinGroups :: [[String]] -> [String]
joinGroups = map concat

sortString = map (sort) 

removeDup (x:y:xs) 
    | x == y = removeDup (y:xs)
    | otherwise = x:removeDup (y:xs)
removeDup x = x