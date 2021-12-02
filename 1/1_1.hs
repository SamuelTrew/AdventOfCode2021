import System.IO

main :: IO()
main = do
    handle <- openFile "1.txt" ReadMode
    contents <- hGetContents handle
    let str = words contents
    let list = readInt str

    print (calc list)
    hClose handle

calc :: [Int] -> Int
calc (x : x' : xs)
    | x' > x = 1 + calc (x':xs)
    | otherwise = calc (x':xs)
calc _ = 0

readInt :: [String] -> [Int]
readInt = map read