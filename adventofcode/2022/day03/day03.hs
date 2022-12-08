import Data.Char (isLower, ord)

solve :: [Int] -> Int
solve = undefined

splitMiddle :: String -> (String, String)
splitMiddle xs = (xl, xr)
  where mid = div (length xs) 2
        xl = take mid xs
        xr = drop mid xs

findCommonChar :: (String, String) -> Char
findCommonChar ((x:xs), ys)
  | elem x ys = x
  | otherwise = findCommonChar (xs, ys)

priority :: Char -> Int
priority c
  | isLower c = ord c - (ord 'a') + 1
  | otherwise = ord c - (ord 'A') + 27

findCommonChar3 :: (String, String, String) -> Char
findCommonChar3 ((x:xs), ys, zs)
  | elem x ys && elem x zs = x
  | otherwise = findCommonChar3 (xs, ys, zs)

part1 :: [String] -> Int
part1 = sum . map (priority . findCommonChar . splitMiddle)

toTuple3 :: [String] -> (String, String, String)
toTuple3 [x, y, z] = (x, y, z)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = (take n xs) : (chunksOf n (drop n xs))

part2 :: [String] -> Int
part2 = sum . map (priority . findCommonChar3 . toTuple3) . chunksOf 3

main :: IO ()
main = interact $ unlines . map show . sequence [part1, part2] . lines
