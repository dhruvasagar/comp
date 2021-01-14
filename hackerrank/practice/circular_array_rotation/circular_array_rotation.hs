cshiftr :: Int -> [Int] -> [Int]
cshiftr k ns
  | k > n = cshiftr (mod k n) ns
  | otherwise = rs ++ ls
  where n = length ns
        ls = take (n - k) ns
        rs = drop (n - k) ns

query :: Int -> [Int] -> [Int] -> [Int]
query k ns = map (ss !!)
  where ss = cshiftr k ns

solve :: [Int] -> [Int]
solve (n:k:q:nss) = query k ns qs
  where ns = take n nss
        qs = drop n nss

main :: IO ()
main = interact $ unlines . map show . solve . map read . words
