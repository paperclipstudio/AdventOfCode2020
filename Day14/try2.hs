import Data.Bits
import qualified Data.Map as Map

main = do
    handle <- readFile "data"
    let file = lines handle
        (mask, memory) = run newState file
        totalSum = sum $ map snd $ Map.toList memory
    print $ memory
    print $ totalSum


type State = (String, Map.Map Int Int)
newState :: State
newState = ("", Map.empty)
run :: State -> [String] -> State

run s [] = s
run s@(mask, memory) (x:xs) = 
    let 
        newState = if take 3 x == "mem" 
            then
                let 
                    key = read $ takeWhile (/= ']') $ drop 4 x
                    value = read $ drop 2 $ dropWhile (/= '=') x
                    newValue = applyMask mask value
                in
                (mask, Map.insert key newValue memory)
            else
                let
                    newMask = drop 7 x
                in
                (newMask, memory)
    in
        run newState xs

applyMask :: String -> Int -> Int
applyMask [] value = value
applyMask (m:ms) value
    |m == 'X' = thisBit * bitValue + applyMask ms rest
    |m == '1' = bitValue + applyMask ms rest
    |m == '0' = applyMask ms rest
    where 
        rest = value `mod` bitValue
        bitValue = round $ 2 ^ length ms
        thisBit = value `div` bitValue