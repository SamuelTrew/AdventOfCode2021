import System.IO
import Data.List.Split (splitOn)
import Data.List (groupBy, sortOn)
import qualified Data.Map as Map
import Data.Char (isUpper)


main :: IO()
main = do
   handle <- openFile "12.txt" ReadMode
   contents <- hGetContents handle

   let connections = map (\x -> (head x, x!!1)) $ map (splitOn "-") (lines contents)
   let rev = map (\x -> (x!!1, head x)) $ map (splitOn "-") (lines contents)
   let nodes = map combine $ buildTree (connections++rev)
   let mp = buildMap nodes
   let paths = findPaths mp "start" []

   print $ length paths

   hClose handle


definiteLookup :: Maybe [String] -> [String]
definiteLookup Nothing = []
definiteLookup (Just s) = s


findPaths :: Map.Map String [String] -> String -> [String] -> [[String]]
findPaths mp current seen
   | current == "end" = [newSeen]
   | elem current seen && isSeen current = []
   | otherwise = concatMap (\x -> findPaths mp x newSeen) next
   where
      next = definiteLookup $ Map.lookup current mp
      newSeen = seen++[current]


isSeen :: String -> Bool
isSeen = not . all isUpper


buildMap :: [(String, [String])] -> Map.Map String [String]
buildMap nodes = Map.fromList nodes


buildTree :: [(String, String)] -> [[(String, String)]]
buildTree connections = groupBy (\x y -> fst x == fst y) $ (sortOn fst) connections


combine :: [(String, String)] -> (String, [String])
combine node = (fst $ head node, map snd node)
