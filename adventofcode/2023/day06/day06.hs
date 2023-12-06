import Data.List (find, intercalate)
import Debug.Trace (trace)

winCount :: (Int, Int) -> Int
winCount (time, dist) = 1 + time - (2 * index)
  where
    (Just index) = find (\t -> dist < (t * (time - t))) [0 .. time]

part1 :: [String] -> Int
part1 [ts, ds] = product $ map winCount $ zip times dists
  where
    times = map read $ tail $ words ts
    dists = map read $ tail $ words ds

part2 :: [String] -> Int
part2 [ts, ds] = winCount (time, dist)
  where
    time = read $ intercalate "" $ tail $ words ts
    dist = read $ intercalate "" $ tail $ words ds

main :: IO ()
main = interact $ unlines . map show . sequence [part1, part2] . lines
