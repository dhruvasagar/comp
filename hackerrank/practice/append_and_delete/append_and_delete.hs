firstMismatch :: String -> String -> Int -> Int
firstMismatch [] _ i = i
firstMismatch _ [] i = i
firstMismatch (s:ss) (t:ts) i
  | s /= t = i
  | otherwise = firstMismatch ss ts (i + 1)

canTransform :: String -> String -> Int -> Bool
canTransform s t k
  | s == t = True
  | k == rk = True
  | k >= (sn + tn) = True
  | k < rk = False
  | otherwise = even (k - rk)
    where fidx = firstMismatch s t 0
          sn = length s
          tn = length t
          rk = (sn - fidx) + (tn - fidx)

solve :: [String] -> String
solve [s, t, ks]
  | ct = "Yes"
  | otherwise = "No"
  where k = read ks :: Int
        ct = canTransform s t k

main :: IO ()
main = interact $ solve . words
