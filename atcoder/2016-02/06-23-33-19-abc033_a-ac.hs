main :: IO ()
main = do
  x : xs <- getLine
  putStrLn $ if all (== x) xs then "SAME" else "DIFFERENT"
