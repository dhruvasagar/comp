import Debug.Trace

minimumCost :: Int -> Int -> Int -> Int -> Int -> Int
minimumCost b w bc wc z
  | bc > (wc + z) = b * (wc + z) + w * wc
  | wc > (bc + z) = w * (bc + z) + b * bc
  | otherwise = b * bc + w * wc

solve :: [Int] -> [Int]
solve [] = []
solve (b:w:bc:wc:z:bws)
  | null bws = [minCost]
  | otherwise = minCost : solve bws
  where minCost = minimumCost b w bc wc z

main :: IO ()
main = interact $ unlines . map show . solve . map read . tail . words
