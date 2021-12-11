import System.IO
import Data.Char (digitToInt)


main :: IO()
main = do
   handle <- openFile "9.txt" ReadMode
   contents <- hGetContents handle

   let positions = (map . map) digitToInt (lines contents)

   -- Pad so I don't have to handle edge cases
   let padded = hPad . vPad $ positions
   let res = map succ (convolve padded (1, 1) [])

   print $ sum res

   hClose handle


vPad :: [[Int]] -> [[Int]]
vPad matrix = replicate (length (head matrix)) (maxBound::Int):matrix++[replicate (length (head matrix)) (maxBound::Int)]


hPad :: [[Int]] -> [[Int]]
hPad matrix = [(maxBound::Int):x++[maxBound::Int] | x <- matrix]


convolve :: [[Int]] -> (Int, Int) -> [Int] -> [Int]
convolve matrix (y, x) carry
   | x == length (head matrix) - 1 = convolve matrix (y+1, 1) carry
   | y == length matrix - 1 = carry
   | isSmallest (matrix!!y!!x) [matrix!!y!!(x+1), matrix!!y!!(x-1), matrix!!(y+1)!!x, matrix!!(y-1)!!x] = convolve matrix (y, x+1) ((matrix!!y!!x):carry)
   | otherwise = convolve matrix (y, x+1) carry


isSmallest :: Int -> [Int] -> Bool
isSmallest val = all (val <)
