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
uncurry2 f (x, y) = f x y

symmetric_closure = concatMap $ \(a, b) -> [(a, b), (b, a)]

(!) m k = maybe empty id $ Map.lookup k m

diff xs ys =
    Set.difference (Set.fromList xs) (Set.fromList ys)

main :: IO ()
main = do
    [n, m] <- readLines 1 :: IO [Int]
    es <- replicateM m (readLines 1) :: IO [[Int]]
    let
        fr1 =
            es
            $> map (\xs -> let [a, b] = xs in (a - 1, b - 1))
            $> symmetric_closure
            $> map (\(a, b) -> (a, [b]))
            $> Map.fromListWith List.union
        
        fr2 = fr1 $> Map.map (concatMap (fr1 !))
        
        count i =
            Set.size $ diff (fr2 ! i) (i : fr1 ! i)
    forM_ [0..(n-1)] $ \i -> putStrLn $ show $ count i
