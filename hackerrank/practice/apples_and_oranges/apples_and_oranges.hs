solve :: [Int] -> [Int]
solve (s:t:a:b:m:_:rest) = [as, os]
  where as = length $ filter (\x -> s <= x && x <= t) $ map (+ a) $ take m rest
        os = length $ filter (\x -> s <= x && x <= t) $ map (+ b) $ drop m rest

main :: IO()
main = interact $ unlines . map show . solve . map read . words
