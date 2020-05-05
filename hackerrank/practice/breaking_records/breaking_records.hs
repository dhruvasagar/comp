breakingCount :: [Int] -> Int -> Int -> (Int, Int) -> (Int, Int)
breakingCount [] _ _ c = c
breakingCount (x:xs) mn mx c = breakingCount xs nmn nmx nc
  where nc = ((fst c) + nmxc, (snd c) + nmnc)
        nmxc = if x > mx then 1 else 0
        nmnc = if x < mn then 1 else 0
        nmn = min x mn
        nmx = max x mx

solve :: [Int] -> (Int, Int)
solve (x:xs) = breakingCount xs x x (0, 0)

showTup :: (Show a, Show b) => (a, b) -> String
showTup (a, b) = (show a) ++ " " ++ (show b)

main = interact $ showTup . solve . map read . tail . words
