import System.IO
import Data.Char (digitToInt)
import Data.List (foldl', transpose)

main :: IO()
main = do
    handle <- openFile "3.txt" ReadMode
    contents <- hGetContents handle
    let strs = lines contents

    let gammaVal = map gamma (transpose strs)
    let epsilonVal = map epsilon gammaVal

    print (convert gammaVal * convert epsilonVal)
    hClose handle


gamma :: String -> Char
gamma str
    | majority str > 0 = '1'
    | otherwise = '0'

majority :: [Char] -> Int
majority str = foldl' (+) 0 (map count str)


count :: Char -> Int
count '1' = 1
count '0' = -1
count _ = 0


epsilon :: Char -> Char
epsilon '1' = '0'
epsilon '0' = '1'
epsilon _ = '2'

convert :: String -> Int
convert = foldl' (\acc x -> acc * 2 + digitToInt x) 0
