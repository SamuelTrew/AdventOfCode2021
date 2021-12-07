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

   let moved = map (\x -> abs (((x - best) * ((x+1) - best)) `div` 2)) sorted

   print (foldl' (+) 0 moved)

   hClose handle


median :: [Int] -> Int
median [x] = x
median xs
   | odd len = xs !! mid
   | even len = evenMedian
   where
      len = length xs
      mid = len `div` 2
      evenMedian = (xs!!(mid-1) + xs!!(mid)) `div` 2
median _ = 0