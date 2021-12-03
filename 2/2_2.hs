import System.IO

main :: IO()
main = do
    handle <- openFile "2.txt" ReadMode
    contents <- hGetContents handle
    let cmds = lines contents
    let list = map words cmds
    let tuples = readTuple list

    let tuple = calc tuples 0
    print(fst tuple * snd tuple)
    hClose handle

calc :: [(String, Int)] -> Int -> (Int, Int)
calc [] _ = (0, 0)
calc ((dir, val):xs) aim
    | dir == "up" = calc xs (aim-val)
    | dir == "down" = calc xs (aim+val)
    | dir == "forward" = (val+x, y+(val*aim))
    | otherwise = final
    where
        final = calc xs aim
        x = fst final
        y = snd final

readTuple :: [[String]] -> [(String, Int)]
readTuple list = [(cmd, read val :: Int) | [cmd, val] <- list]