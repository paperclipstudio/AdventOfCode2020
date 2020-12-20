import Data.List.Split
import Data.List
import Data.Char
import Data.Bifunctor

main = do
    handle <- readFile "data"
    let eachLine = lines handle
        eachPassport = map (map words) $ splitOn [""] eachLine
    print $ length $ filter passportCheck $ map createPassport eachPassport

type Passport = [(String, String)]

--createPassport :: [[[String]]] -> Passport
createPassport = (map (first (takeWhile (/= ':')).splitAt 4)).concat

passportCheck :: Passport -> Bool 
passportCheck x = 
    length x == correctLength &&
    any correctBithYear x && 
    any correctIssueYear x &&
    any correctExprationYear x &&
    any correctHeight x &&
    any correctHairColor x &&
    any correctEyeColor x &&
    any correctPid x
    where 
        correctLength = 
            if any ((=="cid").fst) x then 8 else 7
        correctBithYear (key, num) = 
            key == "byr" &&
            read num >= 1920 && read num <= 2002

        correctIssueYear (key, num) =
            key == "iyr" &&
            read num >= 2010 && read num <= 2020

        correctExprationYear (key, num) =
            key == "eyr" &&
            read num >= 2020 && read num <= 2030

        correctHeight (key, num) =
            key == "hgt" &&
            if all isNumber (take 3 num) then
                let height = read (take 3 num)
                in
                    height <= 193 && height >= 150 &&
                    drop 3 num == "cm"
            else all isNumber (take 2 num) &&
                let height = read (take 2 num)
                in
                    height >= 59 && height <= 76 &&
                    drop 2 num == "in"
        
        correctHairColor (key, value) =
            key == "hcl" &&
            head value == '#' &&
            all isHexDigit (drop 1 value)

        correctEyeColor (key, value) = 
            key == "ecl" &&
            elem value ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"] 
                    
        correctPid (key, value) =
            key == "pid" &&
            length value == 9 &&
            all isNumber value