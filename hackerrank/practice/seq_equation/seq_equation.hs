import Data.List

pinv :: [Int] -> Int -> Int
pinv ns y = x + 1
  where (Just x) = elemIndex y ns

solve :: [Int] -> [Int]
solve ns = map (pinv ns . pinv ns) [1..n]
  where n = length ns

main :: IO ()
main = interact $ unlines . map show . solve . map read . tail . words
