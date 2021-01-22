import Data.List

charOR :: Char -> Char -> Char
charOR '0' '0' = '0'
charOR _ _ = '1'

binaryOR :: String -> String -> String
binaryOR = zipWith charOR

count1 :: String -> Int
count1 = foldl f 0
  where f acc c
          | c == '1' = 1 + acc
          | otherwise = acc

solve :: [String] -> [Int]
solve bs = [head max, length max]
  where bors = [count1 $ binaryOR b1s b2s | b1s <- bs, b2s <- bs, b1s < b2s]
        max = last $ group $ sort bors

main :: IO ()
main = interact $ unlines . map show . solve . sort . drop 2 . words
