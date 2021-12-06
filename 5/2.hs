import System.IO
import Data.List.Split (splitOn)

type Point = (Int, Int)


main :: IO()
main = do
   handle <- openFile "5.txt" ReadMode
   contents <- hGetContents handle

   let ls = lines contents
   let moves = [words line | line <- ls]
   let fromTo = coords moves
   let lines = (concat . (map makeLine)) fromTo

   print (findDuplicates lines [] [])

   hClose handle


findDuplicates :: [Point] -> [Point] -> [Point] -> Int
findDuplicates (l:ls) next dupls
   | (not nextL) && (not duplsL) = findDuplicates ls (l:next) dupls
   | nextL && (not duplsL) = 1 + findDuplicates ls next (l:dupls)
   | nextL && duplsL = findDuplicates ls next dupls
   where
      nextL = elem l next
      duplsL = elem l dupls
findDuplicates _ _ _ = 0


makeLine :: (Point, Point) -> [Point]
makeLine ((a1, a2), (b1, b2))
   | a1 == b1 = [(a1, n) | n <- range2]
   | a2 == b2 = [(n, a2) | n <- range1]
   | otherwise = [(range1!!index, range2!!index) | index <- [0..((length range1)-1)]]
   where
      range1 = lineRange a1 b1
      range2 = lineRange a2 b2


lineRange :: Int -> Int -> [Int]
lineRange x y
   | x > y = reverse [y..x]
   | otherwise = [x..y]


coords :: [[String]] -> [(Point, Point)]
coords ([from, _, to]:xs) = [(fakeToReal from, fakeToReal to)]++(coords xs)
coords _ = []


fakeToReal :: String -> (Int, Int)
fakeToReal str = (read x, read y)
      where
         val = splitOn "," str
         x = val!!0
         y = val!!1