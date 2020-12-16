import Data.List.Split
main = do
    handle <- readFile "data"
    let line = lines handle
        startTime = read $ head line :: Int
        buses = map read $ filter (/= "x") $ splitOn "," $ line !! 1 :: [Int]
    print $ nextBus startTime buses


nextBus startTime buses = 
   
    foldl (\x y -> if fst x > fst y then y else x ) (999, 9) 
    $ zip (map (\x -> x - startTime `mod` x) buses) buses 