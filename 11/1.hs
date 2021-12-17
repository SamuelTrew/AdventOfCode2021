import System.IO
import Data.Char (digitToInt)


main :: IO()
main = do
   handle <- openFile "11.txt" ReadMode
   contents <- hGetContents handle

   let energies = (map . map) digitToInt (lines contents)
   let padded = hPad . vPad $ energies
   let (res, c) = stepN padded 100
   mapM print $ res
   print c

   hClose handle


stepN :: [[Int]] -> Int -> ([[Int]], Int)
stepN energies n
   | n <= 0 = (energies, 0)
   | otherwise = (resN, cN + c)
   where
      (res, c) = step energies
      (resN, cN) = stepN res (n-1)


step :: [[Int]] -> ([[Int]], Int)
step energies = ((map . map) reset res, c)
   where
      res = iter ((map . map) succ energies)
      c = sum $ map count res


count :: [Int] -> Int
count [] = 0
count (e:es)
   | e == -1 = 1 + count es
   | otherwise = count es


reset :: Int -> Int
reset energy
   | energy == -1 = 0
   | otherwise = energy


iter :: [[Int]] -> [[Int]]
iter energies
   | energies == res = energies
   | otherwise = iter res
   where
      res = applyFlash energies (1, 1)


applyFlash :: [[Int]] -> (Int, Int) -> [[Int]]
applyFlash energies (y, x)
   -- Bounds checking
   | x == length (head energies) - 1 = applyFlash energies (y+1, 1)
   | y == length energies - 1 = energies
   -- Actual logic
   | energies!!y!!x > 9 = applyFlash (incrAround energies (y, x)) (y, x+1)
   | otherwise = applyFlash energies (y, x+1)


incrAround :: [[Int]] -> (Int, Int) -> [[Int]]
incrAround energies (rangeY, rangeX) = [[shouldIncr energies (y, x) (rangeY, rangeX) | x <- [0..((length $ head energies) - 1)]] | y <- [0..(length energies - 1)]]


shouldIncr :: [[Int]] -> (Int, Int) -> (Int, Int) -> Int
shouldIncr energies (y, x) (rangeY, rangeX)
   | energies!!y!!x == -1 = -1
   | y == rangeY && x == rangeX = -1
   | y <= (rangeY + 1) && y >= (rangeY - 1) && x <= (rangeX + 1) && x >= (rangeX - 1) = (energies!!y!!x) + 1
   | otherwise = energies!!y!!x


vPad :: [[Int]] -> [[Int]]
vPad matrix = replicate (length (head matrix)) (minBound):matrix++[replicate (length (head matrix)) (minBound)]


hPad :: [[Int]] -> [[Int]]
hPad matrix = [(minBound):x++[minBound] | x <- matrix]
