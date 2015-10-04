import Control.Monad
import Control.Applicative ((<$>), (<*>), Alternative, empty)
import Data.Bits
import Data.Array.IO
import Data.Array.MArray
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

($>) = flip ($)
readLines :: (Read a) => Int -> IO [a]
readLines n =
    (concatMap $ fmap read . words) <$> replicateM n getLine
----
solve [] = []
solve (c : cs) = solve_rec c 1 cs

solve_rec c n [] = c : show n
solve_rec c n (c' : s)
    | c == c'    = solve_rec c (n + 1) s
    | otherwise  = solve_rec c n [] ++ solve_rec c' 1 s

main :: IO ()
main = do
    putStrLn . solve =<< getLine
