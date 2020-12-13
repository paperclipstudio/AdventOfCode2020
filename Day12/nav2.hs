
main :: IO ()
main = do
    handle <- readFile "data"
    let line = lines handle
    print $ solve line

type State = (Int, Int, (Int, Int))

move:: State -> String -> State
move (east, north, (wayE, wayN)) line =
    case instruction of
        'N' -> (east, north, (wayE, wayN + value))
        'S' -> (east, north, (wayE, wayN - value))
        'E' -> (east, north, (wayE + value, wayN))
        'W' -> (east, north, (wayE - value, wayN))
        'R' -> (east, north, (newWayE, newWayN))
        'L' -> (east, north, (newWayE, newWayN))
        'F' -> (newEast, newNorth, (wayE, wayN))
        where
            instruction = head line
            value = read $ drop 1 line

            newEast = east + wayE * value
            newNorth = north + wayN * value

            newWayE 
                | line == "R90" || line == "L270" = wayN
                | value == 180 = -wayE
                | otherwise = -wayN

            newWayN
                | line == "R90" || line == "L270" = -wayE
                | value == 180 = -wayN
                | otherwise = wayE

         

solve :: Foldable t => t String -> Int
solve list= 
    abs east + abs north
    where
        (east, north, _) = foldl move (0,0,(10, 1)) list
    
