import Text.Printf

readInt :: String -> Int
readInt = read

solve n =
	(n `div` 3600, (n `mod` 3600) `div` 60, n `mod` 60)

main = do
	n <- fmap readInt getLine
	let (h, m, s) = solve n
	printf "%02d:%02d:%02d\n" h m s
