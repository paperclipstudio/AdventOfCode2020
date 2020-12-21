import Data.List.Split
import qualified Data.Map as Map
main = do
    handle <- readFile "data"
    let line = lines handle
        dataFile = line
    print $ solve $ map readLine dataFile

data FileLine = Memory Memory | Mask Mask
    deriving (Show)
type Mask = [Bit]
type Value = [Bit]

data Bit = X | I | O
    deriving (Show, Read, Eq)
--          Address Data
type Memory = (Int, Value)

type State = (Mask, Map.Map Int Value)


readLine :: String -> FileLine
readLine input =
    case take 3 input of 
        "mem" -> Memory (read "234234",--(drop 4 (head (splitOn "]" input))), 
                    read "234234"(splitOn " = " input !! 1))

        "mas" -> Mask $ map convert $ drop (length "mask = ") input 

        where
            convert c =
                case c of 
                    'X' -> X
                    '0' -> O
                    '1' -> I
                    

apply :: State -> FileLine -> State
apply (mask, memory) (Mask newMask) = (newMask, memory)
apply (mask, memory) (Memory (address, value)) = (mask, newMemory)
    where
        newMemory = Map.insert address (applyMask mask value) memory

applyMask :: Mask -> Value -> Value
applyMask = zipWith (\x y -> if x /= X then x else y) 



solve :: [FileLine] -> State
solve = foldl apply ([], Map.empty)
 
testMem = "mem[48664] = 204086010"
testMask = "mask = X0X01101010001X10X01X1X1101010100000"
