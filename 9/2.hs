import System.IO
import Data.Char (digitToInt)
import Data.List (sort, foldl')
import qualified Data.Set as Set


main :: IO()
main = do
   handle <- openFile "9.txt" ReadMode
   contents <- hGetContents handle

   let positions = (map . map) digitToInt (lines contents)
   let padded = hPad . vPad $ positions
   let res = convolve padded (1, 1) []

   let sizes = [move padded point (padded!!(fst point)!!(snd point) - 1) | point <- res]
   let areas = map (length . Set.fromList) sizes

   print $ foldl' (*) 1 $ take 3 (reverse $ sort areas)

   hClose handle


move :: [[Int]] -> (Int, Int) -> Int -> [(Int, Int)]
move matrix (y, x) prev
   | curr == 9 = []
   | curr <= prev = []
   | otherwise = [(y,x)]++right++left++up++down
   where
      curr = matrix!!y!!x
      right = move matrix (y, x+1) curr
      left = move matrix (y, x-1) curr
      up = move matrix (y-1, x) curr
      down = move matrix (y+1, x) curr


vPad :: [[Int]] -> [[Int]]
vPad matrix = replicate (length (head matrix)) (9):matrix++[replicate (length (head matrix)) (9)]


hPad :: [[Int]] -> [[Int]]
hPad matrix = [(9):x++[9] | x <- matrix]


convolve :: [[Int]] -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
convolve matrix (y, x) carry
   | x == length (head matrix) - 1 = convolve matrix (y+1, 1) carry
   | y == length matrix - 1 = carry
   | isSmallest (matrix!!y!!x) [matrix!!y!!(x+1), matrix!!y!!(x-1), matrix!!(y+1)!!x, matrix!!(y-1)!!x] = convolve matrix (y, x+1) ((y, x):carry)
   | otherwise = convolve matrix (y, x+1) carry


isSmallest :: Int -> [Int] -> Bool
isSmallest val = all (val <)