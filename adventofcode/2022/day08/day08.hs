import Data.Char (digitToInt)

split :: (a -> Bool) -> [a] -> [[a]]
split p xs = f xs []
  where f [] agg = [agg]
        f (y : ys) agg
          | p y = agg : f ys []
          | otherwise = f ys (agg ++ [y])

data Point = Point Int Int
  deriving (Show, Eq)

data Grid = Grid [[Int]]
  deriving (Show, Eq)

size :: Grid -> Int
size (Grid grid) = length grid

height :: Point -> Grid -> Int
height (Point x y) (Grid grid) = (grid !! y) !! x

parseGrid :: [String] -> Grid
parseGrid = Grid . map (map digitToInt)

visible :: Grid -> Point -> Bool
visible grid p@(Point x y)
  | x == n-1 || y == n-1 = True
  | otherwise = left || top || right || bottom
  where n = size grid
        h = height p grid
        left = all f $ zipWith Point [0..(x-1)] (repeat y)
        top = all f $ zipWith Point (repeat x) [0..(y-1)]
        right = all f $ zipWith Point [(x+1)..(n-1)] (repeat y)
        bottom = all f $ zipWith Point (repeat x) [(y+1)..(n-1)]
        f :: Point -> Bool
        f p = height p grid < h

visibleCount :: Grid -> Int
visibleCount grid = length $ filter (visible grid) points
  where n = size grid
        points = [Point x y | x <- [0..(n-1)], y <- [0..(n-1)]]

part1 :: Grid -> Int
part1 = visibleCount

visibleFrom :: Point -> Grid -> Int -> [Point] -> Int
visibleFrom p grid c [] = c
visibleFrom p grid c (p1:ps)
  | height p1 grid < height p grid = visibleFrom p grid (c + 1) ps
  | height p1 grid >= height p grid = c + 1
  | otherwise = c
  where (Point x1 y1) = p

scenicScore :: Point -> Grid -> Int
scenicScore p@(Point x y) grid = product [l, t, r, b]
  where n = size grid
        l = visibleFrom p grid 0 $ zipWith Point (reverse [0..(x-1)]) (repeat y)
        t = visibleFrom p grid 0 $ zipWith Point (repeat x) (reverse [0..(y-1)])
        r = visibleFrom p grid 0 $ zipWith Point [(x+1)..(n-1)] (repeat y)
        b = visibleFrom p grid 0 $ zipWith Point (repeat x) [(y+1)..(n-1)]

scenicScoreMax :: Grid -> Int
scenicScoreMax grid = maximum $ map (\p -> scenicScore p grid) points
  where n = size grid
        points = [Point x y | x <- [0..(n-1)], y <- [0..(n-1)]]

part2 :: Grid -> Int
part2 = scenicScoreMax

main :: IO ()
main = interact $ unlines . map show . sequence [part1, part2] . parseGrid . lines
