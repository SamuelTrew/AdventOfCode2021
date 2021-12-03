import System.IO
import Data.Char (digitToInt)
import Data.List (foldl', transpose)

main :: IO()
main = do
    handle <- openFile "3.txt" ReadMode
    contents <- hGetContents handle
    let strs = lines contents

    let gammaVal = gamma (transpose strs)
    let epsilonVal = epsilon gammaVal

    print (convert gammaVal * convert epsilonVal)
    hClose handle


gamma :: [String] -> String
gamma = map conc


conc :: String -> Char
conc str
    | majority str > 0 = '1'
    | otherwise = '0'

majority :: [Char] -> Int
majority str = foldl' (+) 0 (map count str)


count :: Char -> Int
count '1' = 1
count '0' = -1
count _ = 0


epsilon :: [Char] -> [Char]
epsilon = map epsilonHelper


epsilonHelper :: Char -> Char
epsilonHelper '1' = '0'
epsilonHelper '0' = '1'
epsilonHelper _ = '2'

convert :: String -> Int
convert = foldl' (\acc x -> acc * 2 + digitToInt x) 0
