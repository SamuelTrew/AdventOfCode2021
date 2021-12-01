import System.IO

main :: IO()
main = do
    handle <- openFile "1_1.txt" ReadMode
    contents <- hGetContents handle
    let str = words contents
    let list = readInt str

    calc list
    hClose handle

calc :: [Int] -> IO()
calc (x:y:z:rest) = print (helper (y:z:rest) (x+y+z) 0)
calc _ = print(0)

helper :: [Int] -> Int -> Int -> Int
helper (x:y:z:[]) prev res
    | x+y+z > prev = res + 1
    | otherwise = res
helper (x:y:z:rest) prev res
    | current > prev = helper (y:z:rest) current res+1
    | otherwise = helper (y:z:rest) current res
    where
        current = x + y + z
helper _ _ _ = 0

readInt :: [String] -> [Int]
readInt = map read