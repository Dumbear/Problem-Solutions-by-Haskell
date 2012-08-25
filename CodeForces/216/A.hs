solve :: (Int, Int, Int) -> Int
solve (x, y, z) = x * y + y * z + z * x - x - y - z + 1

readInput :: IO (Int, Int, Int)
readInput = do
    contents <- getContents
    let x:y:z:_ = map read $ words contents
    return (x, y, z)

showOutput :: Int -> IO ()
showOutput c = do
    print c

main :: IO ()
main = do
    input <- readInput
    showOutput $ solve input
