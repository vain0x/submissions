import Control.Monad

main :: IO ()
main = do
    [a, b] <- replicateM 2 getLine
    putStrLn $
        if length a > length b
            then a
            else b
