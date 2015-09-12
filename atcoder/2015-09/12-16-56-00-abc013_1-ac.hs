import System.IO
import qualified Data.Char as Char

main :: IO ()
main = do
    s <- getLine
    let c = head s
    putStrLn $ show $ Char.ord c - Char.ord 'A' + 1
