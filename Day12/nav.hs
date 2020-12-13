
main = do
    handle <- readFile "data"
    let line = lines handle
    print $ solve line


type State = (Int, Int, Int)

move:: State -> String -> State
move (east, north, facing) line =
    case instruction of
        'N' -> (east, north + value, facing)
        'S' -> (east, north - value, facing)
        'E' -> (east + value, north, facing)
        'W' -> (east - value, north, facing)
        'R' -> (east, north, (facing + value + 360) `mod` 360)
        'L' -> (east, north, (facing - value + 360) `mod` 360)
        'F' -> (newEast, newNorth, facing)
        where
            instruction = head line
            value = read $ drop 1 line

            newEast
                |facing == 0 = east + value
                |facing == 180 = east - value
                |otherwise = east

            newNorth
                |facing == 270 = north + value
                |facing == 90 = north - value
                |otherwise = north

solve :: Foldable t => t String -> Int
solve list= 
    abs east + abs north
    where
        (east, north, _) = foldl move (0,0,0) list
    
