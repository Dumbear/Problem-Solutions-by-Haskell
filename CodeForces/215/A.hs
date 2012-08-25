import Data.List

solve :: ([Int], [Int]) -> Int
solve (xs, ys) = length . last . group $ sort [y `div` x | x <- xs, y <- ys, y `mod` x == 0]

readInput :: IO ([Int], [Int])
readInput = do
    contents <- getContents
    let n:ns:m:ms:_ = lines contents
        xs = map read $ take (read n) $ words ns
        ys = map read $ take (read m) $ words ms
    return (xs, ys)

showOutput :: Int -> IO ()
showOutput r = do
    print r

main :: IO ()
main = do
    input <- readInput
    showOutput $ solve input
