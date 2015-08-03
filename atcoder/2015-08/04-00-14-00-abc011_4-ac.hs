import qualified Data.List as List
import qualified Data.Map as Map
import Data.Char
import Data.Ix
import Data.Array.ST
import Data.Array.Unboxed
import Data.Array.IArray
import Data.Array.Base(unsafeRead, unsafeWrite)
import Data.Maybe
import Control.Monad
import Control.Applicative
import Text.Printf
import System.IO

readInt :: String -> Int
readInt = read

mif :: (MonadPlus m) => Bool -> a -> m a
mif cond value = if cond then return value else mzero

-- 整数除算
tryDiv :: Integral a => a -> a -> Maybe a
tryDiv n m = mif (m /= 0 && n `mod` m == 0) $ n `div` m

-- 組み合わせの確率
combiProb n k = combiProb' !! n !! k
combiProb' :: [[Float]]
combiProb' = [1] : [ zipWith (+) ([0] ++ c) (c ++ [0]) | c' <- combiProb', let c = map (/2) c' ]

-- ちょうど k 歩で位置 x にいく確率
probWalk1D :: Int -> Int -> Float
probWalk1D k x =
	maybe 0 id $ do
		r <- (k + x) `tryDiv` 2
		mif (0 <= r && r <= k) $
			combiProb k r

-- ちょうど n 歩で位置 (x, y) にいく確率
probWalk2D :: Int -> (Int, Int) -> Float
probWalk2D n (x, y) =
	sum [ -- 水平に k 歩、垂直に (n-k) 歩動き、(x, y) に到達する確率
		  probWalk1D k x * probWalk1D (n - k) y * combiProb n k
		| k <- [0..n] ]

-- 座標圧縮
solve :: Int -> Int -> (Int, Int) -> Float
solve n d (x, y) =
	maybe 0 id $ do
		x' <- x `tryDiv` d
		y' <- y `tryDiv` d
		return $ probWalk2D n (x', y')

main = do
	[n, d, x, y] <- fmap (map readInt . concatMap words) (replicateM 2 getLine)
	printf "%.16f" $ solve n d (x, y)

test = do
	let cases = [
		(2,  10000000, 10000000, 10000000,  0.125),
		(10, 2,        3,        7,         0.0),
		(11, 8562174,  25686522, 17124348,  0.018174648284912)
		]
	conds <- forM cases (\(n, d, x, y, p) -> do
		let p' = solve n d (x, y)
		when (p /= p') $ do
			putStrLn $
				"case (n, d, x, y) = " ++ show (n, d, x, y) ++ "\n"
				++ "result: " ++ show p'
				++ "\nexpected: " ++ show p
		return $ p == p'
		)
	return $ and conds
