data Chair = Occ | Flo | Emp
    deriving (Eq, Show)

apply func = do
    handle <- readFile "data"
    let line = convert (lines handle)
    putStr  $ show $ func line

at grid x y =
    if (x < 0) || (x >= length grid) || y < 0 || (y >= length (head grid))
        then 
            Flo 
    else 
        grid !! x !! y

test = [
    [Occ,Emp,Occ,Occ,Flo],
    [Occ,Emp,Occ,Occ,Flo],
    [Occ,Emp,Occ,Occ,Flo],
    [Occ,Emp,Occ,Occ,Flo],
    [Flo,Flo,Flo,Flo,Flo]]

test2 = [
    [Occ,Occ,Occ],
    [Occ,Occ,Occ],
    [Occ,Occ,Occ],
    [Occ,Emp,Occ],
    [Flo,Flo,Flo]]
withNeigh grid x y func =
    func $ justNearGrid grid x y

countNumOcc grid x y =
    foldl (\a c-> (if c==Occ then 1 else 0) + a) 0 $ concat $ justNearGrid grid x y

countAllOcc grid =
    foldl (\a c -> (if c== Occ then 1 else 0) + a) 0 $ concat $ grid

justNearGrid grid x y = 
    map (\x1 -> map (\y1 -> at grid (x1+x) (y1+y)) [-1..1])[-1..1]

rules grid x y =
    case at grid x y of
        Occ -> if numOfPeople >= 5 then Emp else Occ
        Emp -> if numOfPeople == 0 then Occ else Emp
        Flo -> Flo
        where
            numOfPeople = countNumOcc grid x y

move grid =
    map (\x -> map (\y -> rules grid x y) [0..length (head grid)-1]) [0.. length grid - 1]

convert = map (map convert)
    where
        convert x = case x of
            '#' -> Occ
            '.' -> Flo
            'L' -> Emp

solve grid =
    if next == grid then 
        countAllOcc grid
    else
        solve next
        where
            next = move grid 