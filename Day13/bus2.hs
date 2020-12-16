import Data.List.Split
main = do
    handle <- readFile "data"
    let line = lines handle
        buses = zip [0..] $ 
                    map (read.(\x -> if x == "x" then "1" else x)) $
                    splitOn "," $ 
                    line !! 1 :: [(Int, Int)]
    print $ (solve buses)


solve buses = head $ filter (checkT buses) [x| y <- [0..], x <- [firstBus * y]]
    where
        longestBus = foldl (+) 0 buses


checkT buses t = all (==0) [x| (shift, bus) <- buses, x <- [(t + shift) `mod` bus]]