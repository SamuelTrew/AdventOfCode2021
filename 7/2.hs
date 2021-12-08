import System.IO
import Data.List.Split (splitOn)
import Data.List (sort, foldl')

main :: IO()
main = do
   handle <- openFile "7.txt" ReadMode
   contents <- hGetContents handle

   let positions = map read (splitOn "," contents)
   let sorted = sort positions
   let best = sum sorted `div` length sorted

   let moved = map (dist best) sorted

   print (foldl' (+) 0 moved)

   hClose handle


dist :: Int -> Int -> Int
dist best x = abs ((abs (x - best) * (abs (x - best) + 1)) `div` 2)
