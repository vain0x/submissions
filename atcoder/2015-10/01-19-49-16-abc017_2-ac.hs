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

yes_no b = if b then "YES" else "NO"

is_choku_lang :: String -> Bool
is_choku_lang "" = True
is_choku_lang ('h' : 'c' : ss) =
    is_choku_lang ss
is_choku_lang (c : ss) =
    c `elem` "oku" && is_choku_lang ss

main = do
    getLine >>= putStrLn . yes_no . is_choku_lang . reverse
