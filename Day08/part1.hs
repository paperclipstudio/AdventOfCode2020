run file = do
    handle <- readFile file
    let instructions = lines handle
    print $ runCorrect instructions

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

ajust line 
    | instruction == "jmp" = "nop"
    | instruction == "nop" = "jmp" ++ drop 3 line
    | otherwise = line
    where
        instruction = take 3 line

mutate :: [String] -> Int -> [String]
mutate lines current =  take current lines 
    ++ ajust (lines !! current) :drop (current + 1) lines

correct lines = (head
                    .filter (\x -> not (doesLoop (State x 0 0))) -- remove all that now loop
                    .map (mutate lines)                          -- mutate this line
                    .filter (\x -> take 3 (lines !! x) /= "acc"))-- remove all that would mutate a "acc" line
                    [1..]

runCorrect :: [String] -> Int
runCorrect lines = runProgram (State (correct lines) 0 0) []

doesLoop :: CPUState -> Bool
doesLoop state = running state []
    where
        running state@(State lines lineNum _) linesRun 
            | lineNum `elem` linesRun = True
            | lineNum >= length lines = False
            | otherwise = running (clock state) (lineNum:linesRun)