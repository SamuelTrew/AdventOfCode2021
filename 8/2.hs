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
   | is1 x = 1 + rest
   | is4 x = 1 + rest
   | is7 x = 1 + rest
   | is8 x = 1 + rest
   | length x == 6 && is0or6or9 x = 1 + rest
   | otherwise = rest
   where
      rest = count xs

isN :: Int -> String -> Bool
isN n s = length s == n


is0or6or9 :: String -> Int
is0or6or9


is4 = isN 4
is7 = isN 3
is8 = isN 7
is1 = isN 2