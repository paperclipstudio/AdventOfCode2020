import Data.List

apply func = do
    handle <- readFile "data"
    let line = map read (lines handle) :: [Int]
    putStr  $ show $ func line

solve xs = ones * threes
    where
    (ones, _ , threes) = helper.sort $ xs

helper [_] = (
    1,0,1);
helper (x:y:xs) = (first + if diff == 1 then 1 else 0,
                    second + if diff == 2 then 1 else 0,
                    third + if diff == 3 then 1 else 0)
    where 
        diff = y - x
        (first, second, third) = helper (y:xs)

-- Tree is going to be of the form where left
-- branch will be the node with the head of list
-- right branch will be with 
-- I.E. [4,6,8] =>
--          []
--        /   \  
--      []     [4]
--    /  \   /    \
--   _   [6]  [4]   [4,6]
--  /\   /\   /\     /\

data Tree =
      Branch [Int] Tree Tree
    | Leaf [Int]


makeTree :: [Int] -> [Int] -> Tree
makeTree ys [] = Leaf ys
makeTree ys (x:xs) = Branch ys (makeTree ys xs) (makeTree (x:ys) xs)

checkValidFrom0to166 xs = checkValid (0:xs ++ [22])

checkValid [] = True;
checkValid [_]= True
checkValid (x:y:xs) 
    | y - x > 3 = False
    | otherwise = checkValid (y:xs)


test :: [Int]
test = [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4]
checkTree (Branch ys l r) = checkValid ys && checkTree l && checkTree r 
checkTree (Leaf ys) = checkValidFrom0to166 ys

trimTree (Leaf x) = 
    if checkValidFrom0to166 x 
        then
            Leaf x
        else
            Leaf []

trimTree (Branch _ (Leaf []) r) = trimTree r 
trimTree (Branch _ l (Leaf [])) = trimTree l 
trimTree (Branch xs l r) = 
    if checkValid xs 
        then 
            Branch xs (trimTree l) (trimTree r) 
        else
            Leaf []

countLeaf :: Tree -> Int
countLeaf (Branch _ l r) = countLeaf l + countLeaf r
countLeaf (Leaf []) = 0
countLeaf (Leaf _) = 1

stringPlusLength string = string ++ show (length string)

hasNull (Branch _ l r) = hasNull l || hasNull r
hasNull (Leaf xs) = null xs

fullTrim tree= if hasNull tree then fullTrim (trimTree tree) else tree


instance Show Tree where
    show (Branch _ l r) = 
        "<" ++ show l 
        ++ "↑>"++ show r ++ "↑"
    show (Leaf []) = "*"
    show (Leaf list) = show list ++ "\n"
    
