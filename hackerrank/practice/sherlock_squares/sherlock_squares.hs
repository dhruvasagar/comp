sqrtInt :: Integer -> Integer
sqrtInt = toInteger . floor . sqrt . fromIntegral

isPerfectSquare :: Integer -> Bool
isPerfectSquare n = sq * sq == n
  where sq = sqrtInt n

countSquares :: Integer -> Integer -> Integer
countSquares a b
  | isPerfectSquare a = bn - an + 1
  | otherwise = bn - an
  where an = sqrtInt a
        bn = sqrtInt b

solve :: [Integer] -> [Integer]
solve (a:b:rs)
  | null rs = [countSquares a b]
  | otherwise = countSquares a b : solve rs

main :: IO ()
main = interact $ unlines . map show . solve . map read . tail . words
