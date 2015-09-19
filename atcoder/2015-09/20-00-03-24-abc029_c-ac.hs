import Control.Monad
main = mapM_ putStrLn . flip replicateM "abc" . read =<< getLine