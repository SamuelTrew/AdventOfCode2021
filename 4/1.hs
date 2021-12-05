import System.IO
import Data.List (elemIndex, transpose, foldl')
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

    let results = [matrixRunner mat moves 5 | mat <- boards]
    let answers = map fst results
    let moveIndices = map snd results
    let bestFinalNumber = foldr1 min moveIndices

    let indexOfEarliest = fromJust $ (elemIndex bestFinalNumber moveIndices)
    let winningMat = boards!!indexOfEarliest
    let total = foldl' (+) 0 [sum (map read row) | row <- winningMat]
    let rest = sum (map read (answers!!indexOfEarliest))
    -- print (earliest)
    print ((total - rest) * bestFinalNumber)

    hClose handle


matrixRunner :: Mat -> [String] -> Int -> ([String], Int)
matrixRunner mat moves index
    | length valid > 0 = (valid, index)
    | otherwise = matrixRunner mat moves (index+1)
    where
        valid = validateMatrix mat (take index moves)


validateMatrix :: Mat -> [String] -> [String]
validateMatrix mat currMoves
    | rowTruth = mat!!(fromJust $ (elemIndex True rowSubset))
    | colTruth = mat!!(fromJust $ (elemIndex True colSubset))
    | ldTruth = leftDiag
    | rdTruth = rightDiag
    | otherwise = []
    where
        rowTruth = any (==True) rowSubset
        colTruth = any (==True) colSubset
        ldTruth = subSet leftDiag currMoves
        rdTruth = subSet rightDiag currMoves
        rowSubset = [subSet row currMoves | row <- mat]
        colSubset = [subSet col currMoves | col <- transpose(mat)]
        leftDiag = getLDiagonal mat 0
        rightDiag = getRDiagonal mat 0


getLDiagonal :: Mat -> Int -> [String]
getLDiagonal _ 5 = []
getLDiagonal mat index = ((mat!!index)!!index):(getLDiagonal mat (index+1))


getRDiagonal :: Mat -> Int -> [String]
getRDiagonal _ 5 = []
getRDiagonal mat index = ((mat!!index)!!(4-index)):(getLDiagonal mat (index+1))


subSet :: [String] -> [String] -> Bool
subSet a b = all (`elem` b) a


createLines :: [String] -> [[String]]
createLines ls = map words (filter (\x -> x /= "") (tail ls))


createMatrices :: [[String]] -> [Mat]
createMatrices (a:b:c:d:e:xs) = [[a,b,c,d,e]]++(createMatrices xs)
createMatrices (_) = []