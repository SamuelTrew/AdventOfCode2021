import System.IO
import Data.List.Split (splitOn)
import Data.List


main :: IO()
main = do
   handle <- openFile "8.txt" ReadMode
   contents <- hGetContents handle

   let positions = map (splitOn "|") (lines contents)

   let inputs = map (words . head) positions
   let sorted = map order inputs
   let codes = map (words . (!!1)) positions

   let results = map concat (convertAll sorted codes)
   print $ sum (map read results)


   hClose handle


convertAll :: [[String]] -> [[String]] -> [[String]]
convertAll (s:ss) (c:cs) = (convert s c 0):(convertAll ss cs)
convertAll _ _ = []


convert :: [String] -> [String] -> Int -> [String]
convert _ [] _ = []
convert list (_:cs) 10 = convert list cs 0
convert list (c:cs) index
   | equalCodes (list!!index) c = (show index):(convert list cs 0)
   | otherwise = convert list (c:cs) (index+1)


equalCodes :: String -> String -> Bool
equalCodes x y = null (x \\ y) && null (y \\ x)


order :: [String] -> [String]
order inputs = [zero,one,two,three,four,five,six,seven,eight,nine]
   where
   zero = theOther (is0or6or9 inputs) [six, nine]
   one = getN 2 inputs
   two = theOther (is2or3or5 inputs) [three, five]
   three = getFromCodes (is2or3or5 inputs) one
   four = getN 4 inputs
   five = get5 (is2or3or5 inputs) six
   six = get6 (is0or6or9 inputs) one
   seven = getN 3 inputs
   eight = getN 7 inputs
   nine = getFromCodes (is0or6or9 inputs) four


getN :: Int -> [String] -> String
getN len inputs = head (filter (\x -> length x == len) inputs)


get5 :: [String] -> String -> String
get5 inputs six = head (filter (\x -> containsChars six x) inputs)


get6 :: [String] -> String -> String
get6 inputs one = head (filter (\x -> not $ containsChars x one) inputs)


getFromCodes :: [String] -> String -> String
getFromCodes inputs codes = head (filter (\x -> containsChars x codes) inputs)


theOther :: [String] -> [String] -> String
theOther total found = head (filter (\x -> not (elem x found)) total)


containsChars :: String -> String -> Bool
containsChars big small = all (\x -> x `elem` big) small


is0or6or9 :: [String] -> [String]
is0or6or9 = filter (\x -> length x == 6)

is2or3or5 :: [String] -> [String]
is2or3or5 = filter (\x -> length x == 5)