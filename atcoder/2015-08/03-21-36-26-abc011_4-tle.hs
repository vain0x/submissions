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

dx = [1, 0, -1, 0]
dy = [0, -1, 0, 1]
neighbors4 = zip dx dy

boundsSquare n = ((0, 0), (n, n))

-- 期待値の盤面を生成する
solve' n = runSTUArray $ do
	bs <- replicateM 2 $ newArray (boundsSquare (n + 1)) (0 :: Float)
	let board = \i -> (bs !! (i `mod` 2))
	
	writeArray (board 0) (0, 0) 1
	forM_ [0..(n - 1)] $ (\i ->
		update i (board i) (board (i + 1))
		)
	return $ board n

-- (k+1) 回ジャンプしたときの期待値を生成する
update k prev_board next_board = do
	forM_ (range $ boundsSquare (k + 1)) (\(i, j) -> do
		--4近傍の総和
		ps <- forM neighbors4 (\(dj, di) ->
			readArray prev_board (abs (i + di), abs(j + dj))
			)
		let q = foldr (+) 0 ps
		writeArray next_board (i, j) (q / 4)
		)

-- 座標圧縮
solve :: Int -> Int -> (Int, Int) -> Float
solve n d (x, y)
	|    x `mod` d /= 0
	  || y `mod` d /= 0  = 0
	| otherwise          =
		let x' = abs $ x `div` d in
		let y' = abs $ y `div` d in
		(solve' n) ! (x', y')

main = do
	[n, d] <- fmap (map readInt . words) getLine
	[x, y] <- fmap (map readInt . words) getLine
	printf "%.16f" $ solve n d (x, y)
