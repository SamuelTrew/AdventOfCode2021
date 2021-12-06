import System.IO
import Data.List.Split (splitOn)
import Data.List (groupBy, sort)

main :: IO()
main = do
   handle <- openFile "6.txt" ReadMode
   contents <- hGetContents handle

   let states = map read (splitOn "," contents)

   print (length (bruteForce states 0))

   hClose handle


bruteForce :: [Int] -> Int -> [Int]
bruteForce states 80 = states
bruteForce states day = bruteForce next (day+1)
   where
      next = update (sub states)


update :: [Int] -> [Int]
update [] = []
update (x:xs)
   | x == -1 = 6:8:next
   | otherwise = x:next
   where
      next = update xs


sub :: [Int] -> [Int]
sub = map (subtract 1)