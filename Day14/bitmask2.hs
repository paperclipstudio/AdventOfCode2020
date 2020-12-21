import qualified Data.Map as Map
import Data.List.Split
import Data.Bits

main = do
    handle <- readFile "data"
    let line = lines handle
        dataFile = line
    print "hello World"


readMask :: [Char] -> [Maybe Int]
readMask input =
        map convert $ drop (length "mask = ") input 
        where
            convert c =
                case c of 
                    'X' -> Nothing
                    '0' -> Just 0
                    '1' -> Just 1

readMemory memory mask input = Map.insert address value memory
    where
        address = drop 4 (head (splitOn "]" input))
        value = read ((splitOn " = " input) !! 1)

applyMask mask =
    applyZero.applyOnes
    where
        applyZero x = 34 .|. x
        applyOnes x = 34 .&. x


convertlistToBits :: [Maybe Int] -> Int
convertlistToBits [Nothing] = 0
convertlistToBits [Just x] = x
convertlistToBits (Just x: xs) = x .|. (convertlistToBits xs `shiftR` 1)
convertlistToBits (Nothing:xs) = convertlistToBits xs `shiftR` 1



testMem = "mem[48664] = 204086010"
testMask = "mask = X0X01101010001X10X01X1X1101010100000"