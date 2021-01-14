slice :: Int  -> [a] -> [a]
slice index ns = bs ++ as
  where (bs, _:as) = splitAt index ns

refund :: Int -> [Int] -> Int -> Maybe Int
refund k ns m
  | m > share = Just (m - share)
  | otherwise = Nothing
  where items = slice k ns
        amt = sum items
        share = div amt 2

solve :: [Int] -> String
solve (_:k:nsm) = maybe "Bon Appetit" show $ refund k nums m
  where nums = init nsm
        m = last nsm

main :: IO ()
main = interact $ solve . map read . words
