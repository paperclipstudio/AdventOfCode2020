
run file = do
    handle <- readFile file
    let instructions = lines handle
        out = runProgram (State instructions 0 0) []
    print out


split line = (take 3 line, read (drop 5 line) * (if line !! 4 == '+' then 1 else (-1)))

data CPUState = State [String] Int Int
clock :: CPUState -> CPUState
clock (State lines lineNum acc)
    | instruction == "acc" = State lines (lineNum + 1) (acc + value) 
    | instruction == "jmp" = State lines (lineNum + value) acc
    | otherwise            = State lines (lineNum + 1) acc 
    where
        (instruction, value) = split $ lines !! lineNum

runProgram :: CPUState -> [Int] -> Int
runProgram state@(State lines lineNum acc) linesRun 
    | lineNum `elem` linesRun = acc
    | lineNum >= length lines = acc
    | otherwise = runProgram (clock state) (lineNum:linesRun)