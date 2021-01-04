slice :: Int  -> [a] -> [a]
slice index ns = bs ++ as
  where (bs, _:as) = splitAt index ns

goodSplit :: [Int] -> Int -> Int -> Bool
goodSplit ns k m = div amt 2 == m
  where paid = slice k ns
        amt = sum paid

refund :: [Int] -> Int -> Int -> Int
refund ns k m = m - share
  where items = slice k ns
        amt = sum items
        share = div amt 2

billDiv :: [Int] -> Int -> Int -> String
billDiv ns k m
  | goodSplit ns k m = "Bon Appetit"
  | otherwise = show $ refund ns k m

solve :: [Int] -> String
solve (n:k:nsm) = billDiv nums k m
  where nums = init nsm
        m = last nsm

main :: IO ()
main = interact $ solve . map read . words
