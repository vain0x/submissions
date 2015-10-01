import Control.Monad
import Control.Applicative ((<$>), (<*>), Alternative, empty)
import Data.Bits
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

($>) = flip ($)

readLines :: (Read a) => Int -> IO [a]
readLines n =
    (concatMap $ fmap read . words) <$> replicateM n getLine
----

main = do
    xss <- replicateM 3 (readLines 1 :: IO [Int])
    print $ List.foldl' (\acc [s, e] -> acc + s * e `div` 10) 0 xss
