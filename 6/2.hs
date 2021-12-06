import System.IO
import Data.List.Split (splitOn)
import Data.List (groupBy, sort)

type Point = (Int, Int)


main :: IO()
main = do
   handle <- openFile "6.txt" ReadMode
   contents <- hGetContents handle

   let states = map read (splitOn "," contents)

   print (length (bruteForce states 0))

   hClose handle


bruteForce :: [Int] -> Int -> [Int]
bruteForce states 256 = states
bruteForce states day = bruteForce next (day+1)
   where
      next = update (sub states)


update :: [Int] -> [Int]
update states = map fix states ++ map (const 8) [1..(count (-1) states)]


sub :: [Int] -> [Int]
sub = map (subtract 1)


fix :: Int -> Int
fix x
   | x == -1 = 6
   | otherwise = x


count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)