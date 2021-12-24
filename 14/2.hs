import System.IO
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.List (group, sort, foldl')
import Debug.Trace


main :: IO()
main = do
   handle <- openFile "14.txt" ReadMode
   contents <- hGetContents handle

   let ls = lines contents
   let template = head ls
   let rules = map ((\x -> (head x, head (x!!1))) . splitOn " -> ") (drop 2 ls)

   let counts = Map.fromList $ initS template
   let ruleMap = Map.fromList rules

   let chain = Map.toList $ growN ruleMap counts 1
   print chain
   let combined = Map.toList $ (update . extract) chain
   print combined
   print $ map (\x -> (fst x, (`div` 2) . (+1) . snd $ x)) (Map.toList . update $ combined++[(head template, 2), (template!!(length template -1), 2)])
   let counts = map ((`div` 2) . (+1) . snd) (Map.toList . update $ combined++[(head template, 2), (template!!(length template -1), 2)])

   print $ maximum counts - minimum counts

   hClose handle


extract :: [(String, Int)] -> [(Char, Int)]
extract [] = []
extract (x:xs) = (x1, p):(x2, p):extract xs
   where
      ([x1, x2], p) = x


-- update :: [(a, Int)] -> Map.Map a Int
update grown = foldl' (flip . uncurry $ Map.insertWith (+)) Map.empty grown


initS :: String -> [(String, Int)]
initS (t:t':ts) = ([t,t'], 1):initS (t':ts)
initS _ = []


growN :: Map.Map String Char -> Map.Map String Int -> Int -> Map.Map String Int
growN _ counts 0 = counts
growN rules counts n = trace (show grown) growN rules (update grown) (n-1)
   where
      grown = concatMap (grow rules counts) (Map.keys counts)


grow :: Map.Map String Char -> Map.Map String Int -> String -> [(String, Int)]
grow rules counts p = [(left, count), (right, count)]
   where
      count = definiteLookup $ Map.lookup p counts
      find = definiteLookup $ Map.lookup p rules
      left = [head p,find]
      right = [find,p!!1]


definiteLookup :: Maybe a -> a
definiteLookup Nothing = error "Nothing"
definiteLookup (Just s) = s
