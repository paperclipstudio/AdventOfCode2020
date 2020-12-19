import Data.List.Split

main = do
    handle <- readFile "data"
    let eachLine = lines handle
        eachPassport = concatMap (map words) $ splitOn [""] eachLine
    print eachPassport

type Passport = [(String, String)]

createPassport :: [[String]] -> [Passport]
createPassport x = [[("invalid", "passport")]]