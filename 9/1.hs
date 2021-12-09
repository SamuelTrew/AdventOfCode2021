import System.IO
import Data.List
import Data.Char (digitToInt)


main :: IO()
main = do
   handle <- openFile "9.txt" ReadMode
   contents <- hGetContents handle

   let positions = (map . map) digitToInt (lines contents)

   let res = convolve positions (0, 0) []

   print $ sum res


   hClose handle


convolve :: [[Int]] -> (Int, Int) -> [Int] -> [Int]
convolve matrix (0,0) carry
   | isSmallest (matrix!!0!!0) [matrix!!0!!1, matrix!!1!!0] = convolve matrix (0, 1) ((matrix!!0!!0):carry)
   | otherwise = convolve matrix (0, 1) carry
convolve matrix (0, x) carry
   | x >= (length (head matrix)) - 1 = convolve matrix (1, 0) carry
   | isSmallest (matrix!!0!!x) [matrix!!0!!(x+1), matrix!!1!!x, matrix!!0!!(x-1)] = convolve matrix (0, x+1) ((matrix!!0!!x):carry)
   | otherwise = convolve matrix (0, x+1) carry
convolve matrix (y, 0) carry
   | y >= (length matrix) - 1 = carry
   | isSmallest (matrix!!y!!0) [matrix!!(y+1)!!0, matrix!!y!!1, matrix!!(y-1)!!0] = convolve matrix (y, 1) ((matrix!!y!!0):carry)
   | otherwise = convolve matrix (y, 1) carry
convolve matrix (y, x) carry
   | x >= (length (head matrix)) - 1 = convolve matrix (y+1, 0) carry
   | isSmallest (matrix!!y!!x) [matrix!!y!!(x+1), matrix!!y!!(x-1), matrix!!(y+1)!!x, matrix!!(y-1)!!x] = convolve matrix (y, x+1) ((matrix!!y!!x):carry)
   | otherwise = convolve matrix (y, x+1) carry


isSmallest :: Int -> [Int] -> Bool
isSmallest val list = all (val <) list
