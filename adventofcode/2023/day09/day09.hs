diffs :: [Int] -> [Int]
diffs [x, y] = [y - x]
diffs (x : y : xs) = (y - x) : diffs (y : xs)

predictNext :: [Int] -> Int
predictNext ns
    | all (== 0) ds = head ns
    | otherwise = (last ns) + predictNext ds
  where
    ds = diffs ns

predictPrev :: [Int] -> Int
predictPrev ns
    | all (== 0) ds = head ns
    | otherwise = (head ns) - predictPrev ds
  where
    ds = diffs ns

part1 :: [[Int]] -> Int
part1 = sum . map predictNext

part2 :: [[Int]] -> Int
part2 = sum . map predictPrev

main :: IO ()
main = interact $ unlines . map show . sequence [part1, part2] . map (map read . words) . lines
