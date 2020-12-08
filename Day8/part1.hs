
run file = do
    handle <- readFile file
    let instructions = lines handle
        out = boot instructions 0 0 []
    print out


split line = (take 3 line, read (drop 5 line) * (if line !! 4 == '+' then 1 else (-1)))
boot lines lineNum acc run
    | lineNum `elem` run = acc
    | length lines <= lineNum = acc
    | instruction == "acc" = boot lines (lineNum + 1) (acc + value) (lineNum:run)
    | instruction == "jmp" = boot lines (lineNum + value) acc (lineNum:run)
    | instruction == "nop" = boot lines (lineNum + 1) acc (lineNum:run)
    | otherwise = acc
    where
        (instruction, value) = split $ lines !! lineNum


        
