chop :: Int -> [Int] -> [[Int]]
chop _ [] = []
chop n ns = rs : chop n es
  where rs = take n ns
        es = drop n ns

type Matrix = [[Int]]
data Pos = Pos Int Int

atPos :: Pos -> Matrix -> Int
atPos (Pos x y) m = (m !! y) !! x

subMatrix :: Pos -> Matrix -> Matrix
subMatrix (Pos x y) = map (drop x) . drop y

invCountP :: Matrix -> Pos -> Int
invCountP m p = length ss
  where sm = subMatrix p m
        sn = atPos p m
        ss = filter (< sn) $ tail $ concat sm

invCount :: Matrix -> Int
invCount m = sum $ map (invCountP m) ps
  where
    size = length m
    ps = [Pos x y | y <- [0..(size-1)], x <- [0..(size-1)]]

solve :: Int -> [Int] -> [Int]
solve t (n:nss)
  | t == 1 || null rs = [invCount ns]
  | otherwise = invCount ns : solve (t-1) rs
  where nn = n * n
        ns = chop n $ take nn nss
        rs = drop nn nss

solveN :: [Int] -> [Int]
solveN (t:ns) = solve t ns

main :: IO ()
main = interact $ unlines . map show . solveN . map read . words
