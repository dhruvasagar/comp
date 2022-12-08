split :: (a -> Bool) -> [a] -> [[a]]
split p xs = f xs []
  where f [] agg = [agg]
        f (y : ys) agg
          | p y = agg : f ys []
          | otherwise = f ys (agg ++ [y])

data Range = Range Int Int
  deriving (Show)

parseRange :: String -> Range
parseRange r = Range l h
  where rs = split (== '-') r
        l = read $ head rs
        h = read $ last rs

parseRangeTuple :: String -> (Range, Range)
parseRangeTuple r = (r1, r2)
  where rs = split (== ',') r
        r1 = parseRange $ head rs
        r2 = parseRange $ last rs

contains :: Range -> Range -> Bool
contains (Range l1 h1) (Range l2 h2) = (l1 <= l2 && h1 >= h2) || (l2 <= l1 && h2 >= h1)

overlap :: Range -> Range -> Bool
overlap (Range l1 h1) (Range l2 h2) = (l1 <= l2 && h1 >= l2) || (l1 >= l2 && l1 <= h2)

part1 :: [String] -> Int
part1 = length . filter (uncurry contains) . map parseRangeTuple

part2 :: [String] -> Int
part2 = length . filter (uncurry overlap) . map parseRangeTuple

main :: IO ()
main = interact $ unlines . map show . sequence [part1, part2] . lines
