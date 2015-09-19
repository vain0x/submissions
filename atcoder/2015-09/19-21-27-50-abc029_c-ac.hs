import Control.Monad
import Control.Applicative
import Data.List

solve :: Int -> [String]
solve n =
    reverse <$> (iterate gen [""] !! n)
    where
        gen xs = do
            x <- xs
            ['a' : x, 'b' : x, 'c' : x]

main = do
    n <- read <$> getLine :: IO Int
    mapM_ putStrLn $ solve n
