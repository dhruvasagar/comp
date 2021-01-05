import Data.List

part1 :: [Int] -> Int
part1 xs = head [x * y | x <- xs, y <- xs, x + y == 2020]

part2 :: [Int] -> Int
part2 xs = head [x * y * z | x <- xs, y <- xs, z <- xs, x + y + z == 2020]

solve :: [Int] -> String
solve xs = unlines $ map show [part1 xs, part2 xs]

main :: IO ()
main = interact $ solve . map read . words
