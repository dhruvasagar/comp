solve :: [Int] -> Int
solve (k:xs) = length [undefined | (i, xi) <- zip [0..] xs,
                                   (j, xj) <- zip [0..] xs,
                                   i < j,
                                   mod (xi + xj) k == 0]

main = interact $ show . solve . map read . tail . words
