import Data.List.Split
import Data.Char

main = do
    handle <- readFile "data"
    let eachLine = lines handle
        eachPassport = map (map words) $ splitOn [""] eachLine
    print $ length $ filter passportCheck $ map createPassport eachPassport

type Passport = [(String, String)]

--createPassport :: [[String]] -> Passport
createPassport = map (splitAt 4).(concat)

passportCheck :: Passport -> Bool 
passportCheck x = 
    length x == correctLength &&
    any ((=="byr:").map toLower.fst) x && 
    any ((=="iyr:").map toLower.fst) x &&
    any ((=="eyr:").map toLower.fst) x &&
    any ((=="hgt:").map toLower.fst) x &&
    any ((=="hcl:").map toLower.fst) x &&
    any ((=="ecl:").map toLower.fst) x &&
    any ((=="pid:").map toLower.fst) x
    where 
        correctLength = if any ((=="cid:").fst) x then 8 else 7