apply func = do
    handle <- readFile "data.txt"
    let line = lines handle
        nums = map words line :: [[String]]
    putStr $ show $ func nums

getRange :: [Char] -> (Int , Int)
getRange xs = (read low, read high) 
    where
        low = takeWhile (/= '-') xs
        high = drop 1 $ dropWhile (/= '-') xs

count x = length.filter (== x) 

isValid [vaild,letter,list] = (countOfLetter <= high) && (countOfLetter >= low)
    where
        (low, high) = getRange vaild
        countOfLetter = count (letter !! 1) list

main = apply $ length.filter (== True).map isValid
