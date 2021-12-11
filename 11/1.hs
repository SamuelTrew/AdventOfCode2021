import System.IO
import Data.Char (digitToInt)
import Data.Array


main :: IO()
main = do
   handle <- openFile "9.txt" ReadMode
   contents <- hGetContents handle

   let a = array (length contents, length contents)
   let energies = a $ (map . map) digitToInt (lines contents)

   hClose handle


step :: Array Int Int -> (Int, Int) -> [[Int]]
step energies point = incr
   where
      incr =  (map . map) succ energies


applyFlash :: [[Int]] -> (Int, Int) -> [[Int]]
applyFlash energies (y, x)
   | energies!!y!!x > 9 = energies


incrAround :: [[Int]] -> (Int, Int) -> [[Int]]
incrAround energies (y, x) = energies