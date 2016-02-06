import Control.Monad
import Control.Applicative ((<$>))
import qualified Data.List as List

main :: IO ()
main = do
  n <- read <$> getLine
  xs <- replicateM n ((\[s, p] -> (s, read p)) . words <$> getLine)
  let total = sum $ map snd xs
  let dom = List.find (\(s, p) -> p > total `div` 2) xs
  let name = case dom of
        Just (s, p) -> s
        Nothing -> "atcoder"
  putStrLn name
