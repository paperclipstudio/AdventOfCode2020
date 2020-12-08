apply func = do
    handle <- readFile "data.txt"
    let line = lines handle
        nums = map words line :: [[String]]
    putStr $ show $ func nums

getRange :: [Char] -> (Int , Int)
getRange xs = let
                low = takeWhile (/= '-') xs
                high = drop 1 $ dropWhile (/= '-') xs
            in (read low, read high)
count x = length.filter (== x) 

isValid [vaild,letter:_,list] = (countOfLetter <= high) && (countOfLetter >= low)
    where
        (low, high) = getRange vaild
        countOfLetter = count letter list

--main = apply $ length.filter (== True).map isValid

-- part 2

isValid2 [valid, letter:_, list] = 
    checkIndex (first-1) /= checkIndex (second -1)
    where
        (first, second) = getRange valid
        checkIndex index
                | index >= length list = False
                | otherwise = (list !! index) == letter

main = apply $ length.filter (== True).map isValid2
