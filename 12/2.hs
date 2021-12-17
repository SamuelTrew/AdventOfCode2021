import System.IO
import Data.List.Split (splitOn)
import Data.List (groupBy, sortOn, nub)
import qualified Data.Map as Map
import Data.Char (isLower)


main :: IO()
main = do
   handle <- openFile "12.txt" ReadMode
   contents <- hGetContents handle

   let split = map (splitOn "-") (lines contents)
   let connections = filterNodes $ map (\x -> (head x, x!!1)) $ split
   let rev = filterNodes $ map (\x -> (x!!1, head x)) $ split

   let nodes = map combine $ buildTree (connections++rev)
   let mp = Map.fromList nodes
   let paths = findPaths mp "start" []

   print $ length paths

   hClose handle


filterNodes :: [(String, String)] -> [(String, String)]
filterNodes = filter (\x -> snd x /= "start") . filter (\x -> fst x /= "end")


definiteLookup :: Maybe [String] -> [String]
definiteLookup Nothing = []
definiteLookup (Just s) = s


findPaths :: Map.Map String [String] -> String -> [String] -> [[String]]
findPaths mp current seen
   | current == "end" = [newSeen]
   | hasDupl (filter (all isLower) newSeen) = []
   | otherwise = concatMap (\x -> findPaths mp x newSeen) next
   where
      next = definiteLookup $ Map.lookup current mp
      newSeen = seen++[current]


hasDupl :: [String] -> Bool
hasDupl lowers = length lowers - length (nub lowers) == 2


buildTree :: [(String, String)] -> [[(String, String)]]
buildTree connections = groupBy (\x y -> fst x == fst y) $ (sortOn fst) connections


combine :: [(String, String)] -> (String, [String])
combine node = (fst $ head node, map snd node)
