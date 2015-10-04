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
main :: IO ()
main = do
    xs <- readLines 1 :: IO [Int]
    print $ (List.sort xs) !! 1
