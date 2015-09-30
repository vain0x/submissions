import Control.Monad
import Control.Applicative ((<$>), (<*>))
import Data.Bits

readLines :: (Read a) => Int -> IO [a]
readLines n =
    (concatMap $ fmap read . words) <$> replicateM n getLine

main :: IO ()
main = do
    [m, d] <- readLines 1 :: IO [Int]
    putStrLn $ if m `mod` d == 0 then "YES" else "NO"
