import Data.List

chop :: Int -> [Int] -> [[Int]]
chop _ [] = []
chop n ns = fs : chop n rs
  where fs = take n ns
        rs = drop n ns

isSortPossible :: Int -> [Int] -> String
isSortPossible n ns
  | null (sns \\ sts) = "Possible"
  | otherwise = "Impossible"
  where cs = chop n ns
        ts = transpose cs
        sns = map sum cs
        sts = map sum ts

solve :: [Int] -> [String]
solve (n:nss)
  | null rs = [cs]
  | otherwise = cs : solve rs
  where nn = n * n
        ns = take nn nss
        rs = drop nn nss
        cs = isSortPossible n ns

main :: IO ()
main = interact $ unlines . solve . map read . tail . words
