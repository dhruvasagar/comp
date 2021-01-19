import Data.List

cutSticks :: [Int] -> [Int]
cutSticks [] = []
cutSticks (n:ns) = rs
  where rs = map (abs . (n -)) $ dropWhile (== n) ns

solve :: [Int] -> [Int]
solve ns = map length $ takeWhile (/= []) $ iterate cutSticks ns

main :: IO ()
main = interact $ unlines . map show . solve . sort . map read . tail . words
