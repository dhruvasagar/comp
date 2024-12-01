import Data.List (sort, transpose)
import Debug.Trace (trace)

vsorteddiffs :: ([Int], [Int]) -> Int -> Int
vsorteddiffs ([], []) r = r
vsorteddiffs ((l:ls), (r:rs)) res = vsorteddiffs (ls, rs) nres
  where nres = res + abs (l - r)

part1 :: [[Int]] -> Int
part1 ns = vsorteddiffs (sort ls, sort rs) 0
  where [ls, rs] = transpose ns

count :: Int -> [Int] -> Int
count n = length . filter (== n)

part2 :: [[Int]] -> Int
part2 ns = foldl (\r l -> r + (l * (count l rs))) 0 ls
  where [ls, rs] = transpose ns

main :: IO ()
main = interact $ unlines . map show . sequence [part1, part2] . map (map read . words) . lines

