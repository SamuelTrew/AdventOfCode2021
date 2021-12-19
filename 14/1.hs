import System.IO
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.List (group, sort)


main :: IO()
main = do
   handle <- openFile "14.txt" ReadMode
   contents <- hGetContents handle

   let ls = lines contents
   let template = head ls
   let rules = map (\x -> (x!!0, x!!1!!0)) $ map (splitOn " -> ") $ drop 2 ls

   let ruleMap = Map.fromList rules

   let chain = growN template ruleMap 10
   let counts = map length $ (group . sort) chain

   print $ (maximum counts) - (minimum counts)

   hClose handle


growN :: String -> Map.Map String Char -> Int -> String
growN template _ 0 = template
growN template rules n = growN newT rules (n-1)
   where
      newT = grow template rules


grow :: String -> Map.Map String Char -> String
grow [] _ = ""
grow [t] _ = [t]
grow (t:t':ts) rules = (t:new:(grow (t':ts) rules))
   where
      new = definiteLookup $ Map.lookup [t, t'] rules


definiteLookup :: Maybe Char -> Char
definiteLookup Nothing = '?'
definiteLookup (Just s) = s