rot :: [Int] -> Int -> [Int]
rot as k = ls ++ ss
  where
    n = length as
    kk = k `mod` n
    ss = take (n - kk) as
    ls = drop (n - kk) as

showA :: [Int] -> String
showA = unwords . map show

solve :: [Int] -> [String]
solve (n:k:rs)
  | null rrs = [showA $ rot ns k]
  | otherwise = showA (rot ns k) : solve rrs
  where ns = take n rs
        rrs = drop n rs

main :: IO ()
main = interact $ unlines . solve . map read . tail . words
