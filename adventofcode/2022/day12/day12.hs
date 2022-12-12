import Data.List (find)
import Data.Char (ord)
import qualified Data.Map as M

data Point = Point Int Int
  deriving (Show, Eq, Ord)

data Grid = Grid 
  { start :: Point
  , end :: Point
  , points :: [String]
  , start_points :: [Point]
  }
  deriving (Show)

parseGrid :: [String] -> Grid
parseGrid ls = Grid start end ls start_points
  where ysize = length ls
        xsize = length $ head ls
        ps = [Point x y | x <- [0..xsize-1], y <- [0..ysize-1]]
        f :: Point -> Char
        f (Point x y) = (ls !! y) !! x
        (Just start) = find ((== 'S') . f) ps
        (Just end) = find ((== 'E') . f) ps
        start_points = start : filter ((== 'a') . f) ps

bounds :: Grid -> (Int, Int)
bounds (Grid { points }) = (xsize, ysize)
  where ysize = length points
        xsize = length $ head points

get :: Grid -> Point -> Int
get (Grid { start, end, points }) p@(Point x y)
  | p == start = ord 'a'
  | p == end = ord 'z'
  | otherwise = ord $ (points !! y) !! x

inside :: Grid -> Point -> Bool
inside grid (Point x y) = x >= 0 && x < xsize && y >= 0 && y < ysize
  where (xsize, ysize) = bounds grid

neighbours :: Grid -> Point -> [Point]
neighbours grid (Point x y) = filter (inside grid) ps
  where ps = [ Point x (y -1)
             , Point (x - 1) y
             , Point (x + 1) y
             , Point x (y + 1)
             ]

dist :: Grid -> Point -> Point -> Int
dist grid p1 p2 = (get grid p2) - (get grid p1)

visitMany :: [Point] -> M.Map Point Bool -> M.Map Point Bool
visitMany [] vis = vis
visitMany (p:ps) vis = visitMany ps (M.insert p True vis)

findShortestPathLength :: Grid -> [(Point, Int)] -> M.Map Point Bool -> Int
findShortestPathLength grid [] vis = maxBound :: Int
findShortestPathLength grid ((p, steps):ps) vis
  | p == end grid = steps
  | otherwise = findShortestPathLength grid (ps ++ ns) nvis
  where ns = [(n, steps + 1) | n <- neighbours grid p, M.notMember n vis, (<= 1) $ dist grid p n]
        nvis = visitMany (map fst ns) vis

part1 :: Grid -> Int
part1 grid = findShortestPathLength grid queue vis
  where vis = M.singleton (start grid) True
        queue = [(start grid, 0)]

part2 :: Grid -> Int
part2 grid = minimum $ map (\s -> findShortestPathLength grid [(s, 0)] (M.singleton s True)) $ start_points grid

main = interact $ unlines . map show . sequence [part1, part2] . parseGrid . lines
