import Control.Monad
import Control.Applicative ((<$>), (<*>), Alternative, empty)
import Data.Bits
import Data.Ord
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

($>) = flip ($)
readLines :: (Read a) => Int -> IO [a]
readLines n =
    (concatMap $ fmap read . words) <$> replicateM n getLine
----

main = do
    xs <- readLines 3 :: IO [Int]
    mapM_ print $ map snd $ List.sort $ zip (map Down xs) [1..]
