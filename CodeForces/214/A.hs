solve :: (Int, Int) -> Int
solve (n, m) = length [(x, y) | x <- [0 .. intSqrt n], y <- [0 .. intSqrt m], x^2 + y == n, x + y^2 == m]
    where intSqrt = floor . sqrt . fromIntegral

readInput :: IO (Int, Int)
readInput = do
    contents <- getContents
    let x:y:_ = words contents
        n = read x
        m = read y
    return (n, m)

showOutput :: Int -> IO ()
showOutput c = do
    print c

main :: IO ()
main = do
    input <- readInput
    showOutput $ solve input
