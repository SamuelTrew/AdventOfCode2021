import System.IO
import Data.List (foldl', sort)


main :: IO()
main = do
   handle <- openFile "10.txt" ReadMode
   contents <- hGetContents handle

   let statements = lines contents
   let remainders = filter (/= []) (map (isIncomplete []) statements)
   let scores = map score remainders

   print $ (sort scores)!!(length scores `div` 2)

   hClose handle


score :: String -> Int
score r = foldl' (\acc c -> (acc*5) + (getPoints c)) 0 r


isIncomplete :: String -> [Char] -> [Char]
isIncomplete opened (s:ss)
   | isOpening s = isIncomplete (s:opened) ss
   | s == getClosing (head opened) = isIncomplete (tail opened) ss
   | otherwise = []
isIncomplete opened [] = opened


getPoints :: Char -> Int
getPoints '(' = 1
getPoints '[' = 2
getPoints '{' = 3
getPoints '<' = 4
getPoints _ = 0


isOpening :: Char -> Bool
isOpening c = getClosing c /= '0'


getClosing :: Char -> Char
getClosing '(' = ')'
getClosing '{' = '}'
getClosing '[' = ']'
getClosing '<' = '>'
getClosing _ = '0'