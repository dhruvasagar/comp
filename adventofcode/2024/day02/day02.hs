import Data.List (find)
import Debug.Trace (trace)
import Data.Maybe (fromMaybe)

data Order = Inc | Dec

is_safe :: [Int] -> Int -> Int -> Order -> Bool
is_safe [] _ _ _ = False
is_safe (r1:r2:rs) min max Inc
  | r2 < r1 = False
  | dr < min || dr > max = False
  | length rs == 0 = True
  | otherwise = is_safe (r2:rs) min max Inc
  where
    dr = r2 - r1
is_safe (r1:r2:rs) min max Dec
  | r1 < r2 = False
  | dr < min || dr > max = False
  | length rs == 0 = True
  | otherwise = is_safe (r2:rs) min max Dec
  where
    dr = r1 - r2

gen_comb :: [Int] -> Int -> [Int]
gen_comb rs idx
    | idx == 0 = tail rs
    | otherwise = (take idx rs) ++ (drop (idx + 1) rs)

gen_combs :: [Int] -> [[Int]]
gen_combs rs = [gen_comb rs i | i <- [0..n]]
  where n = (length rs) - 1

is_safe_damp :: [Int] -> Int -> Int -> Bool
is_safe_damp rs min max = fromMaybe False $ find (== True) $ map safe $ gen_combs rs
  where safe :: [Int] -> Bool
        safe rs = is_safe rs min max (order rs)
        order :: [Int] -> Order
        order (r1:r2:_)
          | r1 < r2 = Inc
          | otherwise = Dec

part1 :: [[Int]] -> Int
part1 ns = length $ filter (== True) $ map safe ns
  where order :: [Int] -> Order
        order (r1:r2:_)
          | r1 < r2 = Inc
          | otherwise = Dec
        safe :: [Int] -> Bool
        safe rs = is_safe rs 1 3 (order rs)

part2 :: [[Int]] -> Int
part2 ns = length $ filter (== True) $ map safe ns
  where safe :: [Int] -> Bool
        safe rs = is_safe_damp rs 1 3

main :: IO ()
main = interact $ unlines . map show . sequence [part1, part2] . map (map read . words) . lines
