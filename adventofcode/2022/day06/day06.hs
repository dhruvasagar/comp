import Data.List (nub)

uniqueSubstring :: Int -> Int -> String -> Int
uniqueSubstring _ _ [] = 0
uniqueSubstring n i ss
  | n == length (nub (take n ss)) = i + n
  | otherwise = uniqueSubstring n (i + 1) (tail ss)

part1 :: String -> Int
part1 = uniqueSubstring 4 0

part2 :: String -> Int
part2 = uniqueSubstring 14 0

main :: IO ()
main = interact $ unlines . map show . sequence [part1, part2]
