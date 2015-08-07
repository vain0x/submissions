import Control.Monad
import Control.Applicative
import Data.Traversable (sequenceA)
import qualified Data.Map.Strict as Map
import Data.Bits
import Data.Char
import Data.Ix
import Data.Array.IO
import Data.Array.ST
import Data.Array.Unboxed
import Data.Array.IArray
import Data.Array.Base(unsafeRead, unsafeWrite)
import Data.Maybe
import Text.Printf
import System.IO

readInt :: String -> Int
readInt = read

infinity :: Int
infinity = (maxBound :: Int) `div` 4

warshallFroyd dist n =
	forM_ (range (0, n - 1)) $ (\k ->
		forM_ (range ((0, 0), (n - 1, n - 1))) $ (\(i, j) -> do
			d_i_j <- readArray dist (i, j)
			d_i_k <- readArray dist (i, k)
			d_k_j <- readArray dist (k, j)
			let d' = d_i_k + d_k_j
			when (d_i_j > d') $
				writeArray dist (i, j) d'
		))

solve dist n = do
	maxs <- forM [0..(n - 1)] (\i ->
		-- i から各点への距離の最大値
		maximum <$> sequenceA [ readArray dist (i, j) | j <- [0..(n - 1)] ]
		)
	return $ minimum maxs

main = do
	[n, m] <- fmap (map readInt . words) getLine
	dist <- (newArray ((0, 0), (n - 1, n - 1)) infinity) :: IO (IOUArray (Int, Int) Int)
	-- グラフの読み込み
	replicateM_ m $ do
		[i', j', t] <- fmap (map readInt . words) getLine
		let (i, j) = (i' - 1, j' - 1)
		writeArray dist (i, j) t
		writeArray dist (j, i) t -- 逆辺
	-- 自分自身への最短距離は 0
	forM_ [0..(n - 1)] (\i ->
		writeArray dist (i, i) 0
		)
	
	warshallFroyd dist n
	res <- solve dist n
	putStrLn $ show res
