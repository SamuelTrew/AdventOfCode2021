import System.IO

main :: IO()
main = do
    handle <- openFile "2.txt" ReadMode
    contents <- hGetContents handle
    let cmds = lines contents
    let list = map words cmds
    let tuples = readTuple list

    let tuple = calc tuples
    print(fst tuple * snd tuple)
    hClose handle

calc :: [(String, Int)] -> (Int, Int)
calc [] = (0, 0)
calc ((dir, val):xs)
    | dir == "up" = (x, -val+y)
    | dir == "down" = (x, val+y)
    | dir == "forward" = (val+x, y)
    | otherwise = calc xs
    where
        final = calc xs
        x = fst final
        y = snd final

readTuple :: [[String]] -> [(String, Int)]
readTuple list = [(cmd, read val :: Int) | [cmd, val] <- list]