import System.IO
import Data.List.Split (splitOn)
import Data.List (sort, foldl')

main :: IO()
main = do
   handle <- openFile "8.txt" ReadMode
   contents <- hGetContents handle

   let positions = map (splitOn "|") (lines contents)
   let codes = map (words . (!!1)) positions

   print (foldl' (+) 0 (map count codes))


   hClose handle


count :: [String] -> Int
count [] = 0
count (x:xs)
   | is1 x || is4 x || is7 x || is8 x = 1 + rest
   | otherwise = rest
   where
      rest = count xs

isN :: Int -> String -> Bool
isN n s = length s == n

is4 = isN 4
is7 = isN 3
is8 = isN 7
is1 = isN 2