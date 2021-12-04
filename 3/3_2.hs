import System.IO
import Data.Char (digitToInt)
import Data.List (foldl', transpose)

main :: IO()
main = do
    handle <- openFile "3.txt" ReadMode
    contents <- hGetContents handle
    let strs = lines contents

    let oxygen = repeatF strs 0 True
    let co2 = repeatF strs 0 False
    print ((convert oxygen) * (convert co2))

    hClose handle


repeatF :: [String] -> Int -> Bool -> String
repeatF (x:[]) _ _ = x
repeatF strs index isOxygen =
    repeatF (newList strs index isOxygen) (index+1) isOxygen


newList :: [String] -> Int -> Bool -> [String]
newList strs index isOxygen =
    filter (\n -> n!!index == bit) strs
    where
        bit
            | isOxygen = pBit
            | otherwise = carbon pBit
            where
                pBit = primaryBit ((transpose strs)!!index)


primaryBit :: String -> Char
primaryBit str
    | majority str >= 0 = '1'
    | otherwise = '0'

majority :: [Char] -> Int
majority str = foldl' (+) 0 (map count str)


count :: Char -> Int
count '1' = 1
count '0' = -1
count _ = 0


carbon :: Char -> Char
carbon '1' = '0'
carbon '0' = '1'
carbon _ = '2'


convert :: String -> Int
convert = foldl' (\acc x -> acc * 2 + digitToInt x) 0
