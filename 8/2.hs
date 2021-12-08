import System.IO
import Data.List.Split (splitOn)
import Data.List (sort, foldl', elemIndex)

main :: IO()
main = do
   handle <- openFile "8.txt" ReadMode
   contents <- hGetContents handle

   let positions = map (splitOn "|") (lines contents)

   let inputs = map (words . head) positions
   let sorted = map order inputs
   let codes = map (words . (!!1)) positions

   print $ convertAll sorted codes


   hClose handle


convertAll :: [[String]] -> [[String]] -> [String]
convertAll (s:ss) (c:cs) = (convert s c):(convertAll ss cs)
convertAll _ _ = []


convert :: [String] -> [String] -> String
convert sorted codes = concat $ map (\x -> show (elemIndex x sorted)) codes


order :: [String] -> [String]
order inputs = (zero:one:two:three:four:five:six:seven:eight:nine:[])
   where
   zero = getWithinCodes (is0or6or9 inputs) eight
   one = getN 2 inputs
   two = theOther (is2or3or5 inputs) [three, five]
   three = getFromCodes (is2or3or5 inputs) one
   four = getN 4 inputs
   five = getWithinCodes (is2or3or5 inputs) six
   six = theOther (is0or6or9 inputs) [zero, nine]
   seven = getN 3 inputs
   eight = getN 7 inputs
   nine = getFromCodes (is0or6or9 inputs) four


getN :: Int -> [String] -> String
getN len inputs = head (filter (\x -> length x == len) inputs)


getFromCodes :: [String] -> String -> String
getFromCodes inputs codes = head (filter (\x -> containsChars x codes) inputs)


theOther :: [String] -> [String] -> String
theOther total found = head (filter (\x -> not (elem x found)) total)


getWithinCodes :: [String] -> String -> String
getWithinCodes inputs codes = head (filter (\x -> containsChars codes x) inputs)


containsChars :: String -> String -> Bool
containsChars big small = all (\x -> x `elem` big) small


is0or6or9 :: [String] -> [String]
is0or6or9 = filter (\x -> length x == 6)

is2or3or5 :: [String] -> [String]
is2or3or5 = filter (\x -> length x == 5)