finalChair :: Int -> Int -> Int -> Int
finalChair n m s = last $ take m $ drop (s - 1) $ cycle [1..n]

finalChair2 :: Int -> Int -> Int -> Int
finalChair2 n m s
  | rem == 0 = n
  | otherwise = rem
  where rem = mod (m + s - 1) n

solve :: [Int] -> [Int]
solve (n:m:s:nss)
  | null nss = [finalChair2 n m s]
  | otherwise = finalChair2 n m s : solve nss

main :: IO ()
main = interact $ unlines . map show . solve . map read . tail . words
