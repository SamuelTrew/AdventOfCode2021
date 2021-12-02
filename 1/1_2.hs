import System.IO

main :: IO()
main = do
    handle <- openFile "1.txt" ReadMode
    contents <- hGetContents handle
    let str = words contents
    let list = readInt str

    -- print (helper (tail list) (head(list)+head(tail(list))+list!!2))
    calc list
    hClose handle

calc :: [Int] -> IO()
calc (x:y:z:rest) = print (helper (y:z:rest) (x+y+z))
calc _ = print 0

helper :: [Int] -> Int -> Int
helper (x:y:z:rest) prev
    | current > prev = 1 + helper (y:z:rest) current
    | otherwise = helper (y:z:rest) current
    where
        current = x + y + z
helper _ _ = 0

readInt :: [String] -> [Int]
readInt = map read