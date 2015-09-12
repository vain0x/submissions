import System.IO
import qualified Data.Char as Char
import Control.Monad
import Control.Applicative ((<$>), (<*>))

readInts :: Int -> IO [Int]
readInts n =
    (concatMap $ fmap (read :: String -> Int) . words) <$> replicateM n getLine

solve a b =
    minimum $ map (\n -> if n < 0 then n + 10 else n)
        [ (b - a)
        , (a + (10 - b))
        , (a - b)
        , (b + (10 - a))
        ]

main :: IO ()
main = do
    [a, b] <- readInts 2
    print $ solve a b
