import qualified Data.List as List
import qualified Data.Map as Map
import Data.Char
import Data.Array
import Data.Maybe
import Control.Monad
import Control.Applicative
import Text.Printf
import System.IO

readInt :: String -> Int
readInt = read

solve k n ngs times
	| k == n            = True
	| times == 0        = False
	| canAdd 3 k n ngs  = solve (k + 3) n ngs (times - 1)
	| canAdd 2 k n ngs  = solve (k + 2) n ngs (times - 1)
	| canAdd 1 k n ngs  = solve (k + 1) n ngs (times - 1)
	| otherwise         = False
	where
		canAdd inc k n ngs = (k + inc) <= n && not ((k + inc) `elem` ngs)

main = do
	(n: ngs) <- fmap (map readInt) $ replicateM 4 getLine
	putStrLn $ (if solve 0 n ngs 100 then "YES" else "NO")
