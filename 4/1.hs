import System.IO
import Data.List (elemIndex, transpose, foldl', intersect)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)


type Mat = [[String]]

main :: IO()
main = do
    handle <- openFile "4.txt" ReadMode
    contents <- hGetContents handle

    let ls = lines contents
    let moves = splitOn "," (head ls)
    let boards = createMatrices (createLines ls)

    let results = [matrixRunner mat moves 4 | mat <- boards]
    let moveIndices = map snd results
    let bestIndex = foldr1 min moveIndices
    let bestFinalNumber = read (moves!!bestIndex)

    let indexOfEarliest = fromJust $ (elemIndex bestIndex moveIndices)
    let winningMat = boards!!indexOfEarliest
    let total = foldl' (+) 0 [sum (map read row) | row <- winningMat]
    let m = sum (map read (marked winningMat (take (bestIndex+1) moves)))

    print ((total - m) * bestFinalNumber)

    hClose handle


marked :: Mat -> [String] -> [String]
marked [] _ = []
marked (x:xs) moves = (intersect x moves)++(marked xs moves)


matrixRunner :: Mat -> [String] -> Int -> ([String], Int)
matrixRunner mat moves index
    | length valid > 0 = (valid, index)
    | otherwise = matrixRunner mat moves (index+1)
    where
        valid = validateMatrix mat (take (index+1) moves)


validateMatrix :: Mat -> [String] -> [String]
validateMatrix mat currMoves
    | rowTruth = mat!!(fromJust $ (elemIndex True rowSubset))
    | colTruth = mat!!(fromJust $ (elemIndex True colSubset))
    | otherwise = []
    where
        rowTruth = any (==True) rowSubset
        colTruth = any (==True) colSubset
        rowSubset = [subSet row currMoves | row <- mat]
        colSubset = [subSet col currMoves | col <- transpose(mat)]


subSet :: [String] -> [String] -> Bool
subSet a b = all (`elem` b) a


createLines :: [String] -> [[String]]
createLines ls = map words (filter (\x -> x /= "") (tail ls))


createMatrices :: [[String]] -> [Mat]
createMatrices (a:b:c:d:e:xs) = [[a,b,c,d,e]]++(createMatrices xs)
createMatrices (_) = []