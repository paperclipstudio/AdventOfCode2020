import Data.List

apply func = do
    handle <- readFile "data"
    let line = map read (lines handle) :: [Int]

    putStr $ show $ func line

solve xs = ones * threes
    where
    (ones, _ , threes) = helper.sort $ xs

helper [_] = (
    1,0,1);
helper (x:y:xs) = (first + if diff == 1 then 1 else 0,
                    second + if diff == 2 then 1 else 0,
                    third + if diff == 3 then 1 else 0)
    where 
        diff = y - x
        (first, second, third) = helper (y:xs)

