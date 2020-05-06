import Data.Ord
import Data.List
import Data.Function

mostCommon :: Ord a => [a] -> a
-- mostCommon = snd . head . maximum . groupBy (\a b -> fst a == fst b) . map (\xs -> (length xs, head xs)) . group . sort
mostCommon = head . head . sortBy (flip compare `on` length) . group . sort

solve :: [Int] -> String
solve = show . mostCommon

main :: IO ()
main = interact $ solve . map read . tail . words
