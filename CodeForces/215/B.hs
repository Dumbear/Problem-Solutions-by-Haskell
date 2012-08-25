solve :: ([Double], [Double], [Double], Double, Double) -> Double
solve (xs, ys, zs, a, b) = sqrt $ r1^2 * p1 * b / (p1 * b + p2 * a)
    where r1 = maximum xs
          p1 = maximum ys
          p2 = minimum zs

readInput :: IO ([Double], [Double], [Double], Double, Double)
readInput = do
    contents <- getContents
    let ns:ms:ks:ab:_ = lines contents
        xs = map read $ tail $ words ns
        ys = map read $ tail $ words ms
        zs = map read $ tail $ words ks
        a = read $ words ab !! 0
        b = read $ words ab !! 1
    return (xs, ys, zs, a, b)

showOutput :: Double -> IO ()
showOutput r = do
    print r

main :: IO ()
main = do
    input <- readInput
    showOutput $ solve input
