-- https://www.hackerrank.com/challenges/reverse-factorization
import Data.Functor
import Data.List
import Control.Monad
import Data.Ord

selections :: [a] -> [(a,[a])]
selections [] = []
selections (x:xs) = (x,xs) : selections xs

solve :: Int -> [Int] -> [[Int]]
solve n arr = go [] where
  go acc = do
    i <- arr
    guard (if null acc then True else i >= head acc)
    let ret = i:acc
    guard (product ret <= n)
    if (product ret == n) then return ret
    else go ret

lexCmp :: Ord a => [a] -> [a] -> Ordering
lexCmp [] _ = LT
lexCmp _ [] = GT
lexCmp (x:xs) (y:ys) = case compare x y of
  EQ -> lexCmp xs ys
  GT -> GT
  LT -> LT

fmt :: [[Int]] -> String
fmt [] = "-1"
fmt xs = unwords $ map show $ scanl (*) 1 $ minimumBy (lexCmp) $ map reverse $ xs

main = do
  [n,k] <- map read <$> words <$> getLine
  arr <- map read <$> words <$> getLine
  putStr $ fmt $ solve n arr
  -- print $ solve n arr
