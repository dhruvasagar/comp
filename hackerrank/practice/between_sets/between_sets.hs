-- fold :: (a -> a -> a) -> [a] -> a
-- fold f [] = error "list is empty"
-- fold f [x] = x
-- fold f (x:xs) = f x (fold f xs)

between :: [Int] -> [Int] -> Int
between a b = length
          $ filter (\x -> mod divs x == 0)
          $ takeWhile (<= divs)
          $ map (* lcma) [1..]
  where lcma = foldl1 (lcm) a
        divs = foldl1 (gcd) b

solve :: [Int] -> Int
solve (n:_:rest) = between a b
  where a = take n rest
        b = drop n rest

main = interact $ show . solve . map read . words
