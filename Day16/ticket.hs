import Data.List.Split
import Data.Char
import Data.List

main = do
    handle <- readFile "data"
    let line = lines handle
    let (types: rest) = splitOn [""] line
    let vaildRanges = (getRange.clearType) types
    --print $ filter (isAlpha.head) $ (map (words)) $ concat rest
    print $sum $ sort $ filter (not.checkAll vaildRanges) $ getAllTicketNums rest

getAllTicketNums x = map read $ concatMap (splitOn ",") $ filter (isDigit.head) $ concat x
clearType :: [String] -> [String]
clearType = map (\x -> splitOn ": " x !! 1)

--getRange :: [String] -> [String]
getRange ::  [String] -> [(Int, Int)]
getRange x = map ((\[x,y] -> (read x,read y)).splitOn "-") $ concatMap (splitOn " or " ) x


checkAll :: [(Int, Int)] -> Int -> Bool
checkAll ((low1, high1):(low2, high2):xs) num
    | (low1 <= num && num <= high1) || (low2 <= num && num <= high2) = True
    |otherwise = checkAll xs num

checkAll _ _ = False;

