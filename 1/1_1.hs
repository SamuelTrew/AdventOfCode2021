import System.IO

main :: IO()
main = do
    handle <- openFile "1_1.txt" ReadMode
    contents <- hGetContents handle
    let str = words contents
    let list = readInt str

    calc list
    hClose handle

calc :: [Int] -> IO()
calc list = print (helper list 0)

helper :: [Int] -> Int -> Int
helper [] _ = 0
helper (_:[]) _ = 0
helper (x:y:[]) res
    | y > x = res+1
    | otherwise = res
helper (x : x' : xs) res
    | x' > x = helper (x':xs) res+1
    | otherwise = helper (x':xs) res

readInt :: [String] -> [Int]
readInt = map read