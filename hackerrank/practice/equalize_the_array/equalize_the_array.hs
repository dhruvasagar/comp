import Data.List
import Data.Function

solve :: [Int] -> Int
solve = length . concat . tail . sortBy (flip compare `on` length) . group

main :: IO ()
main = interact $ show . solve . sort . map read . tail . words
