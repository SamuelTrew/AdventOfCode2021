import System.IO
import Data.List.Split (splitOn)
import Data.List (groupBy, sort)

type Point = (Int, Int)


main :: IO()
main = do
   handle <- openFile "5.txt" ReadMode
   contents <- hGetContents handle

   let ls = lines contents
   let moves = [words line | line <- ls]
   let fromTo = coords moves
   let connections = (concat . (map makeLine)) fromTo
   let sorted = (groupTuples . sort) connections

   print (findDuplicates sorted)

   hClose handle

groupTuples :: [Point] -> [[Point]]
groupTuples = groupBy (\a b -> fst a == fst b && snd a == snd b)


findDuplicates :: [[Point]] -> Int
findDuplicates [] = 0
findDuplicates (x:xs)
   | length x > 1 = 1 + (findDuplicates xs)
   | otherwise = findDuplicates xs


makeLine :: (Point, Point) -> [Point]
makeLine ((a1, a2), (b1, b2))
   | a1 == b1 = [(a1, n) | n <- (lineRange (a2) (b2))]
   | a2 == b2 = [(n, a2) | n <- (lineRange (a1) (b1))]
   | otherwise = []


lineRange :: Int -> Int -> [Int]
lineRange x y
   | x > y = [y..x]
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