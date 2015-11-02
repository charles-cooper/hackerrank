-- https://www.hackerrank.com/challenges/jumping-bunnies
import Data.Functor

main = do
  getLine
  arr <- map read <$> words <$> getLine
  print $ foldr1 lcm arr
