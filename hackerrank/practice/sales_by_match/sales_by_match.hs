import Data.List

countPair :: [Int] -> Int
countPair [] = 0
countPair [_] = 0
countPair (n1:n2:ns)
  | n1 == n2 = 1 + countPair ns
  | otherwise = countPair (n2:ns)

main :: IO ()
main = interact $ show . countPair . sort . map read . tail . words
