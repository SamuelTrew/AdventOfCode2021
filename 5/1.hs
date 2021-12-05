import System.IO
import Data.List (elemIndex, transpose, foldl', intersect)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)


main :: IO()
main = do
    handle <- openFile "5.txt" ReadMode
    contents <- hGetContents handle

    let ls = lines contents
    let moves = [words line | line <- ls]
    let fromTo = coords moves
    print fromTo

    hClose handle


coords :: [[String]] -> [((Int, Int), (Int, Int))]
coords ([from, _, to]:xs) = [(fakeToReal from, fakeToReal to)]++(coords xs)
coords _ = []


fakeToReal :: String -> (Int, Int)
fakeToReal str = (read x, read y)
      where
         val = splitOn "," str
         x = val!!0
         y = val!!1