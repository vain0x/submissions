import Data.Bits

readLnIntList :: IO [Int]
readLnIntList = fmap (fmap read . words) getLine

main = do
    [n, x] <- readLnIntList
    xs <- readLnIntList
    putStrLn $ show $
        sum [if testBit x i then xs !! i else 0 | i <- [0..(n - 1)]]