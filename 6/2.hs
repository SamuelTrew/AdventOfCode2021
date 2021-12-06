import System.IO
import Data.List.Split (splitOn)
import Data.List (group, sort, foldl')

main :: IO()
main = do
   handle <- openFile "6.txt" ReadMode
   contents <- hGetContents handle

   let states = (map read (splitOn "," contents))::[Int]
   let pairs = (group . sort) states
   let groups = [0]++(map length pairs)++[0, 0, 0]

   print (foldl' (+) 0 (iterator groups 0))

   hClose handle


shifter :: [Int] -> [Int]
shifter (x0:x1:x2:x3:x4:x5:x6:x7:x8) = (x1:x2:x3:x4:x5:x6:(x7+x0):(head x8):x0:[])


iterator :: [Int] -> Int -> [Int]
iterator counts 256 = counts
iterator counts index = iterator (shifter counts) (index+1)

