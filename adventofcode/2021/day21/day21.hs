import Debug.Trace (trace)

dice :: [Int]
dice = cycle [1..100]

score :: [Int] -> Int
score ss
  | mod sm 10 == 0 = 10
  | sm > 10 = mod sm 10
  | otherwise = sm
  where sm = sum ss


game :: [Int] -> [Int] -> [Int] -> Int -> Int
game [] _ _ _ = 0
game [p1, p2] [s1, s2] rs n
  | s1n >= 1000 = s2 * (nn - 3)
  | s2n >= 1000 = s1 * nn
  | otherwise = game [p1n, p2n] [s1n, s2n] rrs nn
  where p1n = score $ p1 : take 3 rs
        p2n = score $ p2 : take 3 (drop 3 rs)
        s1n = s1 + p1n
        s2n = s2 + p2n
        rrs = drop 6 rs
        nn = n + 6

solve :: [Int] -> Int
solve ps = game ps [0, 0] dice 0

main :: IO ()
main = interact $ show . solve . map (read . last . words) . lines

