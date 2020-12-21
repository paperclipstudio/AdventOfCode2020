import Data.List

main = do
    handle <- readFile "data"
    let seatNums = map getSeatNum $ lines handle
        mySeat = (getMissing.sort) seatNums
        largest = foldl (\x y -> if x > y then x else y) 0 seatNums
    print mySeat

getMissing (x:y:xs) = if x + 1 == y then 
        getMissing (y:xs)
    else
        x + 1


getRow :: String -> Int
getRow x =
    sum $ zipWith (*) info $ reverse [power2s | ones <- [0..6], power2s <- [round (2**ones)]]
    where
        info = map (\c-> if c == 'B' then 1 else 0) $ take 7 x 

getColumn x =
    sum $ zipWith (*) info $ reverse [power2s | ones <- [0..2], power2s <- [round (2**ones)]]
    where
        info = map (\c-> if c == 'R' then 1 else 0) $ drop 7 x 

getSeatNum x = (8 * getRow x) + getColumn x
test1 = "FBFBBFFRLR"
test2 = "BFFFFFFRRR"