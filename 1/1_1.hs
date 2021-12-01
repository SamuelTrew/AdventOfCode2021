import System.IO
import Control.Monad

main = do
        let list = []
        handle <- openFile "1_1.txt" ReadMode
        contents <- hGetContents handle
        let singlewords = words contents
            list = f singlewords
        calc list
        hClose handle

f :: [String] -> [Int]
f = map read

calc :: [Int] -> Int
calc list =
    do
        let result = helper list 0
        print (nth (read result :: Integer))

helper :: [Int] -> Int -> Int
helper (x:[]) res = res
helper (x : x' : xs) res
    | x' > x = helper (x':xs) res+1
    | otherwise = helper (x':xs) res

