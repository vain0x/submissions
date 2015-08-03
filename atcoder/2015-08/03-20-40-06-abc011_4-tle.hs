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

boundsSquare n = ((-n, -n), (n, n))

updateMArray marr i f = do
	x <- readArray marr i
	writeArray marr i (f x)

transformMArray marr f = do
	bounds <- getBounds marr
	forM_ (range bounds) (\i -> updateMArray marr i f)

-- 期待値の盤面を生成する
solve' n = runSTUArray $ do
	board0 <- newArray (boundsSquare $ (n + 1)) (0::Float)
	board1 <- newArray (boundsSquare $ (n + 1)) (0::Float)
	let board = \i ->
		if (i `mod` 2) == 0 then board0 else board1
	writeArray board0 (0, 0) 1.0
	forM_ [0..(n - 1)] $ (\i ->
		update i (board i) (board (i + 1))
		)
	return $ board n

-- (k+1) 回ジャンプしたときの期待値を生成する
update k prev_board next_board = do
	transformMArray next_board (const 0)
	forM_ (range $ boundsSquare k) (\(i, j) -> do
		p <- readArray prev_board (i, j)
		forM_ (zip dy dx) (\(di, dj) -> do
			updateMArray next_board (i + di, j + dj) ((+) (p / 4))
			)
		)

-- 座標を圧縮する
solve n d (x, y)
	|    x `mod` d /= 0
	  || y `mod` d /= 0  = 0
	| otherwise          = (solve' n) ! (x `div` d, y `div` d)

main = do
	[n, d] <- fmap (map readInt . words) getLine
	[x, y] <- fmap (map readInt . words) getLine
	printf "%.16f" $ solve n  d (x, y)
