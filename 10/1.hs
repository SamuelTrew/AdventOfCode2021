import System.IO
import Data.List (foldl')


main :: IO()
main = do
   handle <- openFile "10.txt" ReadMode
   contents <- hGetContents handle

   let statements = lines contents

   print $ foldl' (+) 0 [isValid s [] | s <- statements]

   hClose handle


isValid :: String -> [Char] -> Int
isValid (s:ss) opened
   | isOpening s = isValid ss (s:opened)
   | s == getClosing (head opened) = isValid ss (tail opened)
   | otherwise = getPoints s
isValid _ _ = 0


getPoints :: Char -> Int
getPoints ')' = 3
getPoints ']' = 57
getPoints '}' = 1197
getPoints '>' = 25137
getPoints _ = 0


isOpening :: Char -> Bool
isOpening '(' = True
isOpening '{' = True
isOpening '[' = True
isOpening '<' = True
isOpening _ = False


getClosing :: Char -> Char
getClosing '(' = ')'
getClosing '{' = '}'
getClosing '[' = ']'
getClosing '<' = '>'
getClosing _ = '0'