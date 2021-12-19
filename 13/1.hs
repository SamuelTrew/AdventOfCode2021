import System.IO
import Data.List.Split (splitOn)
import Data.List (groupBy, sortOn)
import qualified Data.Map as Map
import Data.Char (isUpper)
import Data.Set (fromList)


main :: IO()
main = do
   handle <- openFile "13.txt" ReadMode
   contents <- hGetContents handle


   let [coords, commands] = map lines $ splitOn "\n\n" contents
   let points = map (\x -> (read (x!!0) :: Int, read (x!!1) :: Int)) $ map (splitOn ",") coords
   let cmds = (map (\x -> (splitOn " " x)!!2) commands)

   let res = folding cmds points
   print $ length $ fromList res

   hClose handle


folding :: [String] -> [(Int, Int)] -> [(Int, Int)]
folding [] points = points
folding (c:cmds) points
   | dir == "x" = folding cmds (foldX points (read line))
   | otherwise = folding cmds (foldY points (read line))
   where
      [dir, line] = splitOn "=" c


foldX :: [(Int, Int)] -> Int -> [(Int, Int)]
foldX [] _ = []
foldX (p:ps) line
   | x > line = ((line - (x - line), y):rest)
   | otherwise = (p:rest)
   where
      (x,y) = p
      rest = foldX ps line


foldY :: [(Int, Int)] -> Int -> [(Int, Int)]
foldY [] _ = []
foldY (p:ps) line
   | y > line = ((x, line - (y - line)):rest)
   | otherwise = (p:rest)
   where
      (x,y) = p
      rest = foldY ps line
