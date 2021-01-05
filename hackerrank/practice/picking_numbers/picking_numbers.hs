import Data.List

isGood :: Int -> Int -> Bool
isGood a b
  | abs (a - b) <= 1 = True
  | otherwise = False

subGood :: [Int] -> [Int]
subGood [] = []
subGood [a] = [a]
subGood (a:as) = a:ss
  where ss = [x | x <- as, isGood a x]

solve :: [Int] -> Int
solve [] = 0
solve as@(a:ar)
  | otherwise = maximum [sn, solve ar]
  where
    ss = subGood as
    sn = length ss

main :: IO ()
main = interact $ show . solve . sort . map read . tail . words
