import System.IO
import Data.List
import Data.Char (digitToInt)
import Debug.Trace


main :: IO()
main = do
   handle <- openFile "9.txt" ReadMode
   contents <- hGetContents handle

   let positions = (map . map) digitToInt (lines contents)
   let res = map succ (convolve positions (0, 0) [])

   print $ sum res


   hClose handle


convolve :: [[Int]] -> (Int, Int) -> [Int] -> [Int]
convolve matrix (0,0) carry
   | isSmallest (matrix!!0!!0) [matrix!!0!!1, matrix!!1!!0] = convolve matrix (0, 1) ((matrix!!0!!0):carry)
   | otherwise = convolve matrix (0, 1) carry
convolve matrix (0, x) carry
   | x == (length (head matrix)) - 1 = topRight matrix (0, x) carry
   | isSmallest (matrix!!0!!x) [matrix!!0!!(x+1), matrix!!1!!x, matrix!!0!!(x-1)] = convolve matrix (0, x+1) ((matrix!!0!!x):carry)
   | otherwise = convolve matrix (0, x+1) carry
convolve matrix (y, 0) carry
   | y == (length matrix) - 1 = bottomLeft matrix (y, 0) carry
   | isSmallest (matrix!!y!!0) [matrix!!(y+1)!!0, matrix!!y!!1, matrix!!(y-1)!!0] = convolve matrix (y, 1) ((matrix!!y!!0):carry)
   | otherwise = convolve matrix (y, 1) carry
convolve matrix (y, x) carry
   | x == (length (head matrix)) - 1 && y == (length matrix) - 1 = bottomRight matrix (y, x) carry
   | x == (length (head matrix)) - 1 = rightSide matrix (y, x) carry
   | y == (length matrix) - 1 = bottom matrix (y, x) carry
   | isSmallest (matrix!!y!!x) [matrix!!y!!(x+1), matrix!!y!!(x-1), matrix!!(y+1)!!x, matrix!!(y-1)!!x] = convolve matrix (y, x+1) ((matrix!!y!!x):carry)
   | otherwise = convolve matrix (y, x+1) carry


isSmallest :: Int -> [Int] -> Bool
isSmallest val list = all (val <) list


topRight :: [[Int]] -> (Int, Int) -> [Int] -> [Int]
topRight matrix (_, x) carry
   | isSmallest (matrix!!0!!x) [matrix!!1!!x, matrix!!0!!(x-1)] = convolve matrix (1, 0) ((matrix!!0!!x):carry)
   | otherwise = convolve matrix (1, 0) carry


bottomLeft :: [[Int]] -> (Int, Int) -> [Int] -> [Int]
bottomLeft matrix (y, _) carry
   | isSmallest (matrix!!y!!0) [matrix!!y!!1, matrix!!(y-1)!!0] = convolve matrix (y, 1) ((matrix!!y!!0):carry)
   | otherwise = convolve matrix (y, 1) carry


rightSide :: [[Int]] -> (Int, Int) -> [Int] -> [Int]
rightSide matrix (y, x) carry
   | isSmallest (matrix!!y!!x) [matrix!!(y-1)!!x, matrix!!(y+1)!!x, matrix!!y!!(x-1)] = convolve matrix (y+1, 0) ((matrix!!y!!x):carry)
   | otherwise = convolve matrix (y+1, 0) carry


bottom :: [[Int]] -> (Int, Int) -> [Int] -> [Int]
bottom matrix (y, x) carry
   | isSmallest (matrix!!y!!x) [matrix!!(y-1)!!x, matrix!!y!!(x+1), matrix!!y!!(x-1)] = convolve matrix (y, x+1) ((matrix!!y!!x):carry)
   | otherwise = convolve matrix (y, x+1) carry


bottomRight :: [[Int]] -> (Int, Int) -> [Int] -> [Int]
bottomRight matrix (y, x) carry
   | isSmallest (matrix!!y!!x) [matrix!!y!!(x-1), matrix!!(y-1)!!x] = ((matrix!!y!!x):carry)
   | otherwise = carry
