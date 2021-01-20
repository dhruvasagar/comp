jump :: [Int] -> [Int]
jump [] = []
jump [_] = []
jump [x, y] = [y]
jump [x, y, z] = [z]
jump (x:y:xs)
  | z == 0 = xs
  | otherwise = y:xs
  where z = head xs

solve :: [Int] -> Int
solve = length . tail . takeWhile (/= []) . iterate jump

main :: IO ()
main = interact $ show . solve . map read . tail . words
