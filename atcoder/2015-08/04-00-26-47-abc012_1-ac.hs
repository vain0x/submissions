main = getLine >>= (putStrLn . unwords . reverse . words)