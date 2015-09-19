import Control.Monad

main = do
    ss <- replicateM 12 getLine
    let n = length $ filter ('r' `elem`) ss
    putStrLn $ show n
