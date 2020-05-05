solve :: [Int] -> String
solve [x1, v1, x2, v2]
  | vdiff == 0 && xdiff == 0 = "YES"
  | xdiff /= 0 && vdiff == 0 = "NO"
  | xdiff < 0 && vdiff < 0 || xdiff > 0 && vdiff > 0 = "NO"
  | mod (abs xdiff) (abs vdiff) /= 0 = "NO"
  | otherwise = "YES"
  where xdiff = x1 - x2
        vdiff = v1 - v2

main = interact $ solve . map read . words
