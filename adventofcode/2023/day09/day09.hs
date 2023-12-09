diffs :: [Int] -> [Int]
diffs xs = zipWith subtract xs (tail xs)

next :: [Int] -> Int
next = sum . map last . takeWhile (any (/= 0)) . iterate diffs

part1 :: [[Int]] -> Int
part1 = sum . map next

part2 :: [[Int]] -> Int
part2 = sum . map (next . reverse)

main :: IO ()
main = interact $ unlines . map show . sequence [part1, part2] . map (map read . words) . lines
