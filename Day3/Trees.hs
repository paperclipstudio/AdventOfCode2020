apply func = do
    handle <- readFile "data"
    let line = lines handle

    putStr $ show $ func line

width = 30
tree = '#'

hit line x = line !! (x `mod` length line) == tree

solve2 = flip solve 0

solve [] _ _  = 0
solve list@(x:_) shift vec@(right, down) = 
    solve (drop down list) (shift + right) vec + 
    fromEnum (hit x shift)

part2 [] _ = 1;
part2 (x:xs) input = part2 xs input
                    * solve input 0 x 

part2Solve = part2 [(1,1), (3,1), (5,1), (7,1), (1, 2)]