import System.IO
import qualified Data.Char as Char
import Data.Bits
import Data.Maybe
import Data.Array
import Control.Monad
import Control.Monad.Fix
import Control.Applicative ((<$>), (<*>))
import Data.Foldable (fold)

readLines :: (Read a) => Int -> IO [a]
readLines n =
    (concatMap $ fmap read . words) <$> replicateM n getLine

readLinesInt :: Int -> IO [Int]
readLinesInt = readLines

intmax = 1 `shiftL` 29

main :: IO ()
main = do
    args <- readLinesInt 2
    let [n, h, a, b, c, d, e] = args
    let dfs i sat
            | i == n    = 0
            | otherwise = dfs_memo !! i !! sat
        dfs_memo =
            [ [dfs' i j | j <- [0..]] | i <- [0..n] ]
        dfs' i sat =
            minimum
                [ a + dfs (i + 1) (sat + b)
                , c + dfs (i + 1) (sat + d)
                , if sat > e then dfs (i + 1) (sat - e) else intmax ]
        result = dfs 0 h
    print $ result