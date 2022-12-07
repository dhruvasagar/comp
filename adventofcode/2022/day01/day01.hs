import Data.List (sort)

split :: (a -> Bool) -> [a] -> [[a]]
split p xs = f xs []
  where f [] agg = [agg]
        f (y : ys) agg = if p y
                         then agg : f ys []
                         else f ys (agg ++ [y])


part1 :: [String] -> Int
part1 = maximum . map (sum . map read) . split null

part2 :: [String] -> Int
part2 = sum . take 3 . reverse . sort . map (sum . map read) . split null

main :: IO ()
main = interact $ unlines . map show . sequence [part1, part2] . lines
