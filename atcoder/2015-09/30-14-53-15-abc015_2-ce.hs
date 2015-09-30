import Control.Monad

readLines :: (Read a) => Int -> IO [a]
readLines n =
    (concatMap $ fmap read . words) <$> replicateM n getLine

main :: IO ()
main = do
    [n] <- readLines 1 :: IO [Int]
    xs <- readLines 1 :: IO [Int]
    let ys = filter ((/=) 0) xs
    let m = length ys
    print $ (sum ys + (m - 1)) `div` m
