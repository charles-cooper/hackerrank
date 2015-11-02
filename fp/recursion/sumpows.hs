-- https://www.hackerrank.com/challenges/functional-programming-the-sums-of-powers
import Data.Functor
import Control.Monad
import Data.List

pow :: Int -> Int -> Int
pow x 0 = 1
pow x n = x * pow x (n-1)

solve :: Int -> Int -> [[Int]]
solve x n = go (takeWhile (<=x) $ map (`pow`n) [1..x]) []
  where
  go xs acc = do
    i <- xs
    let ret = i : acc
    let ys = dropWhile (<=i) xs -- make sure we only select in ascending order
    guard (sum ret <= x)
    if (sum ret == x)
      then return ret
      else go ys ret

main = do
  x <- read <$> getLine
  n <- read <$> getLine
  print $ length $ solve x n

