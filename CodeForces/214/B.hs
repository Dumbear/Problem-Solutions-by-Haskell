import Data.List

solve :: [Int] -> [Int]
solve ds
    | all (/= 0) ts = []
    | all (== 0) ts = [0]
    | otherwise = reverse ts
    where r = sum ds `mod` 3
          xs = sort ds
          ps = map (\x -> filter (\y -> y `mod` 3 == x) xs) [0, 1, 2]
          ts | r == 0 = xs
             | null $ ps !! r = xs \\ (take 2 (ps !! (3 - r)))
             | otherwise = delete (ps !! r !! 0) xs

readInput :: IO [Int]
readInput = do
    contents <- getContents
    let n:ns = words contents
        ds = map read $ take (read n) ns
    return ds

showOutput :: [Int] -> IO ()
showOutput ds = do
    putStrLn $ if null ds then "-1" else concat $ map show ds

main :: IO ()
main = do
    input <- readInput
    showOutput $ solve input
