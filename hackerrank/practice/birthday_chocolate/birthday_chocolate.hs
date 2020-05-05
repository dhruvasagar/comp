validBar :: [Int] -> Int -> Int -> Int
validBar s d m
  | sum lst == d = 1
  | otherwise = 0
  where lst = take m s

countValidBars :: [Int] -> Int -> Int -> Int -> Int
countValidBars [] d m cnt = cnt
countValidBars s d m cnt = countValidBars ns d m ncnt
  where ns = tail s
        ncnt = cnt + (validBar s d m)

solve :: [Int] -> Int
solve (n:ns) = countValidBars s d m 0
  where s = take n ns
        [d, m] = drop n ns

main = interact $ show . solve . map read . words
