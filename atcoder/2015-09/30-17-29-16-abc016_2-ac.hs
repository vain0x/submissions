import Control.Monad
import Control.Applicative ((<$>), (<*>))
import Data.Bits

readLines :: (Read a) => Int -> IO [a]
readLines n =
    (concatMap $ fmap read . words) <$> replicateM n getLine

main :: IO ()
main = do
    [a, b, c] <- readLines 1 :: IO [Int]
    putStrLn $
        if a == c && b == 0 then "?"
        else if a + b == c then "+"
        else if a - b == c then "-"
        else "!"