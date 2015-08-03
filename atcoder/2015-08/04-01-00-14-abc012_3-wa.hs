import Control.Monad
import Text.Printf

readInt :: String -> Int
readInt = read

totalSum =
	sum [ x * y | x <- [1..9], y <- [1..9] ]

factors n =
	[ k | k <- [1..n], n `mod` k == 0 ]

prods n =
	[ (k, n `div` k) | k <- factors n ]

main = do
	n <- fmap readInt getLine
	forM_ (prods $ totalSum - n) $ (\(i, j) ->
		putStrLn $ show i ++ " x " ++ show j
		)
