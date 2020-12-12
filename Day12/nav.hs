
apply func = do
    handle <- readFile "data"
    let line = (lines handle)
    putStr  $ show $ func line

