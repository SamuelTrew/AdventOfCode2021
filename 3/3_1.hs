import System.IO
import Data.Char (digitToInt)
import Data.List (foldl')

main :: IO()
main = do
    handle <- openFile "3.txt" ReadMode
    contents <- hGetContents handle
    let strs = lines contents

    let gammaVal = gamma strs 0
    let epsilonVal = epsilon gammaVal

    print (convert gammaVal * convert epsilonVal)
    hClose handle


gamma :: [String] -> Int -> [Char]
gamma list index
    | index >= length (head list) = []
    | res > 0 = '1' : next
    | otherwise = '0' : next
    where
        res = getColumnMajority [binary!!index | binary <- list]
        next = gamma list (index+1)


epsilon :: [Char] -> [Char]
epsilon = map epsilonHelper


epsilonHelper :: Char -> Char
epsilonHelper '1' = '0'
epsilonHelper '0' = '1'
epsilonHelper _ = '2'


getColumnMajority :: [Char] -> Int
getColumnMajority [] = 0
getColumnMajority ('1':xs) = getColumnMajority xs + 1
getColumnMajority ('0':xs) = getColumnMajority xs - 1
getColumnMajority (_:xs) = getColumnMajority xs

convert :: String -> Int
convert = foldl' (\acc x -> acc * 2 + digitToInt x) 0
