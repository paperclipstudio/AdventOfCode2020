
apply func = do
    handle <- readFile "data"
    let line = map read (lines handle) :: [Int]

    putStr $ show $ func line


twoSums target (x:xs) 
    | null xs = False
    | (target - x) `elem` xs = True
    | otherwise = twoSums target xs

split index xs = drop (index - 26) $ take index xs

correct xs = twoSums (xs !! 25) (take 25 xs)

firstIncorrect list 
    | correct list = firstIncorrect $ drop 1 list
    | otherwise = list !! 25


large :: Int
large = 1309761972
sumToLarge xs 
    | null isSum = sumToLarge $ drop 1 xs
    | otherwise = sum $ head isSum
    where
        isSum = 
                filter (\x -> sum x == large) $
                takeWhile (\x -> sum x <= large) $
                map (`take` xs) [1..]