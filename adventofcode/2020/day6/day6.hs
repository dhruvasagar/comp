import Data.List

partitionBy :: (a -> Bool) -> [a] -> [[a]]
partitionBy _ [] = []
partitionBy f (x:xs)
  | f x = partitionBy f xs
  | otherwise = (x:ts):partitionBy f rs
  where
    ts = takeWhile (not . f) xs
    rs = drop (length ts) xs

part1 :: [String] -> Int
part1 = sum . map (length . nub . concat) . partitionBy (== "")

part2 :: [String] -> Int
part2 = sum . map (length . foldl1 intersect) . partitionBy (== "")

solve :: [String] -> String
solve ls = unlines $ map show [part1 ls, part2 ls]

main :: IO ()
main = interact $ solve . lines
