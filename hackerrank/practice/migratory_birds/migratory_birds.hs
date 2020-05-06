import Data.Ord
import Data.List

mostCommon :: Ord a => [a] -> a
mostCommon = snd . head . maximum . groupBy (\a b -> fst a == fst b) . map (\xs -> (length xs, head xs)) . group . sort

solve :: [Int] -> String
solve = show . mostCommon

main = interact $ solve . map read . tail . words
