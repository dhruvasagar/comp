solve :: [Int] -> Int
solve (b:n:_:nm) = maximum (-1:[k + d | k <- ks, d <- ds, k + d <= b])
  where ks = take n nm
        ds = drop n nm

main :: IO ()
main = interact $ show . solve . map read . words
