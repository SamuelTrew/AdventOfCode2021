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